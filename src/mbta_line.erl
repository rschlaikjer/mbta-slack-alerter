-module(mbta_line).
-behaviour(gen_server).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").
-compile([{parse_transform, lager_transform}]).

-export([
    start_link/2
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WATCHER_HEARTBEAT, 10000).
-define(TIMEOUT, 30000).

-record(state, {line, color, ets}).

start_link(Line, Color) when is_list(Line) ->
    gen_server:start_link(?MODULE, [Line, Color], []).

init([Line, Color]) ->
    lager:info("Started line alerter for the ~p line~n", [Line]),
    Ets = init_ets(),
    {ok, #state{ets=Ets, line=Line, color=Color}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast({entity, FeedEntity}, State) ->
    handle_feed_entity(State, FeedEntity),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:info("Cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:info("Info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

%% Internal

init_ets() ->
    ets:new(acked_alerts, [set]).

ack_feedentity(Ets, #feedentity{id=Id}) ->
    ets:insert(Ets, {Id, true}).

feed_entity_acked(Ets, #feedentity{id=Id}) ->
    case ets:lookup(Ets, Id) of
        [] -> false;
        _ -> true
    end.

handle_feed_entity(#state{line=Line, ets=Ets, color=Color}, FeedEntity) ->
    case feed_entity_acked(Ets, FeedEntity) of
        true -> ok;
        false ->
            ack_feedentity(Ets, FeedEntity),
            check_and_alert(Line, Color, FeedEntity)
    end.

check_and_alert(Line, Color, FeedEntity) ->
    Alert = FeedEntity#feedentity.alert,
    case alert_affects_route_id(Alert, Line) of
        false -> ok;
        true -> send_alert(Color, Alert)
    end.

alert_affects_route_id(Alert, RouteId) ->
    Entities = Alert#alert.informed_entity,
    lists:any(
        fun(Entity) -> Entity#entityselector.route_id == RouteId end,
        Entities
    ).

send_alert(Color, Alert) ->
    {ok, Channel} = application:get_env(mbta, slack_channel),
    {ok, BotName} = application:get_env(mbta, slack_bot_name),
    {ok, BotIcon} = application:get_env(mbta, slack_bot_icon),
    Attachment = attachment_for_alert(Color, Alert),
    slack_alert(
        Channel,
        BotName,
        BotIcon,
        Attachment
    ),
    ok.

attachment_for_alert(Color, Alert) ->
    Header = Alert#alert.header_text,
    Description = Alert#alert.description_text,
    Timerange = hd(Alert#alert.active_period),
    AlertText = io_lib:format(
        "Alert! ~s until ~s. Cause: ~s.~n~s~n",
        [
            get_translation(Header, "en"),
            format_time(unix_seconds_to_datetime(Timerange#timerange.'end')),
            Alert#alert.cause,
            get_translation(Description, "en")
        ]
    ),
    [
        {<<"fallback">>, erlang:list_to_binary(AlertText)},
        {<<"color">>, Color},
        {<<"title">>, erlang:list_to_binary(io_lib:format(
            "Alert! ~s until ~s. Cause: ~s", [
                get_translation(Header, "en"),
                format_time(unix_seconds_to_datetime(Timerange#timerange.'end')),
                Alert#alert.cause
            ]))
        },
        {<<"text">>, erlang:list_to_binary(get_translation(Description, "en"))}
    ].

get_translation(#translatedstring{translation=Translations}, Language) ->
    case lists:filter(
        fun(Translation) -> Translation#translatedstring_translation.language == Language end,
        Translations
    ) of
        [Translation] -> Translation#translatedstring_translation.text;
        _ -> undefined
    end.

unix_seconds_to_datetime(Seconds) when is_integer(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds + 62167219200);
unix_seconds_to_datetime(_) -> undefined.

format_time({{Y, M, D}, {H, Mi, S}}) ->
    io_lib:format("~2..0b:~2..0b:~2..0b ~b/~2..0b/~2..0b", [H, Mi, S, Y, M, D]);
format_time(_) ->
    "further notice".

slack_alert(Channel, Username, Emoji, Attachment) ->
    Json = jsx:encode([
        {<<"channel">>, Channel},
        {<<"username">>, Username},
        {<<"icon_emoji">>, Emoji},
        {<<"attachments">>, [Attachment]}
    ]),
    Header = [],
    Type = "application/json",
    {ok, SlackUrl} = application:get_env(mbta, slack_url),
    {ok, {{_HttpVer, _Code, _Msg}, _ResponseHeaders, ResponseBody}} =
        httpc:request(post, {SlackUrl, Header, Type, Json}, [], []),
    erlang:iolist_to_binary(ResponseBody).
