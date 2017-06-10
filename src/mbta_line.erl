-module(mbta_line).
-behaviour(gen_server).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").

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

-record(state, {line, color, ets}).

start_link(Line, Color) when is_list(Line) ->
    start_link(list_to_binary(Line), Color);
start_link(Line, Color) when is_binary(Line) ->
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

ack_feedentity(Ets, #'FeedEntity'{id=Id}) ->
    ets:insert(Ets, {Id, true}).

feed_entity_acked(Ets, #'FeedEntity'{id=Id}) ->
    case ets:lookup(Ets, Id) of
        [] -> false;
        _ -> true
    end.

handle_feed_entity(State=#state{line=Line, ets=Ets, color=Color}, FeedEntity) ->
    case feed_entity_acked(Ets, FeedEntity) of
        true -> ok;
        false ->
            ack_feedentity(Ets, FeedEntity),
            check_and_alert(State, FeedEntity)
    end.

check_and_alert(State=#state{line=Line}, FeedEntity) ->
    Alert = FeedEntity#'FeedEntity'.alert,
    AlertId = FeedEntity#'FeedEntity'.id,
    case alert_affects_route_id(Alert, Line) of
        false -> ok;
        true -> send_alert(State, AlertId, Alert)
    end.

alert_affects_route_id(Alert, RouteId) ->
    Entities = Alert#'Alert'.informed_entity,
    lists:any(
        fun(Entity) -> Entity#'EntitySelector'.route_id == RouteId end,
        Entities
    ).

send_alert(#state{color=Color}, AlertId, Alert) ->
    Attachment = attachment_for_alert(Color, Alert),
    gen_server:cast(mbta_slack, {send_alert, AlertId, Attachment}).

attachment_for_alert(Color, Alert) ->
    Header = Alert#'Alert'.header_text,
    Description = Alert#'Alert'.description_text,
    Timerange = hd(Alert#'Alert'.active_period),
    AlertText = io_lib:format(
        "Alert! ~s until ~s. Cause: ~s.~n~s~n",
        [
            get_translation(Header, <<"en">>),
            format_time(unix_seconds_to_datetime(Timerange#'TimeRange'.'end')),
            Alert#'Alert'.cause,
            get_translation(Description, <<"en">>)
        ]
    ),
    [
        {<<"fallback">>, erlang:list_to_binary(AlertText)},
        {<<"color">>, Color},
        {<<"title">>, erlang:list_to_binary(io_lib:format(
            "Alert! ~s until ~s. Cause: ~s", [
                get_translation(Header, <<"en">>),
                format_time(unix_seconds_to_datetime(Timerange#'TimeRange'.'end')),
                Alert#'Alert'.cause
            ]))
        },
        {<<"text">>, get_translation(Description, <<"en">>)}
    ].

get_translation(#'TranslatedString'{translation=Translations}, Language) ->
    case lists:filter(
        fun(Translation) -> Translation#'TranslatedString.Translation'.language == Language end,
        Translations
    ) of
        [Translation] -> Translation#'TranslatedString.Translation'.text;
        _ -> undefined
    end.

unix_seconds_to_datetime(Seconds) when is_integer(Seconds) ->
    calendar:universal_time_to_local_time(
        calendar:gregorian_seconds_to_datetime(Seconds + 62167219200)
    );
unix_seconds_to_datetime(_) -> undefined.

format_time({{Y, M, D}, {H, Mi, S}}) ->
    io_lib:format("~2..0b:~2..0b:~2..0b ~b/~2..0b/~2..0b", [H, Mi, S, Y, M, D]);
format_time(_) ->
    "further notice".
