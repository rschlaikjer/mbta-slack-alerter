-module(mbta_slack).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {ets}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    lager:info("Started mbta slack handler"),
    Ets = init_ets(),
    {ok, #state{ets=Ets}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast({send_alert, AlertId, Attachment}, State) ->
    handle_send_alert(State, AlertId, Attachment),
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
    ets:new(sent_alerts, [set]).

mark_alert_sent(Ets, Id) ->
    ets:insert(Ets, {Id, sent}).

is_alert_sent(Ets, Id) ->
    case ets:lookup(Ets, Id) of
        [{Id, sent}] -> true;
        _ -> false
    end.

handle_send_alert(#state{ets=Ets}, AlertId, Attachment) ->
    case is_alert_sent(Ets, AlertId) of
        true -> ok;
        false ->
            mark_alert_sent(Ets, AlertId),
            send_slack_attachment(Attachment)
    end.

send_slack_attachment(Attachment) ->
    {ok, Channel} = application:get_env(mbta, slack_channel),
    {ok, BotName} = application:get_env(mbta, slack_bot_name),
    {ok, BotIcon} = application:get_env(mbta, slack_bot_icon),
    Json = jsx:encode([
        {<<"channel">>, Channel},
        {<<"username">>, BotName},
        {<<"icon_emoji">>, BotIcon},
        {<<"attachments">>, [Attachment]}
    ]),
    Header = [],
    Type = "application/json",
    {ok, SlackUrl} = application:get_env(mbta, slack_url),
    {ok, {{_HttpVer, _Code, _Msg}, _ResponseHeaders, ResponseBody}} =
        httpc:request(post, {SlackUrl, Header, Type, Json}, [], []),
    erlang:iolist_to_binary(ResponseBody).
