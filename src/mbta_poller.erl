-module(mbta_poller).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").

-define(MBTA_ALERT_URL, "http://developer.mbta.com/lib/GTRTFS/Alerts/Alerts.pb").
-define(HEARTBEAT_MILLIS, 600000).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
    start_link/0,
    fetch_mbta_alerts/0
]).

-record(state, {

}).

%% Gen server callbacks
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    lager:info("Starting MBTA poller"),
    reset_heartbeat(),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    lager:info("Call ~p From ~p", [Request, From]),
    {reply, ignored, State}.

handle_cast(heartbeat, State) ->
    reset_heartbeat(),
    fetch_and_cast_alerts(),
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


%% MBTA related data

fetch_and_cast_alerts() ->
    Data = fetch_mbta_alerts(),
    Entities = Data#feedmessage.entity,
    lists:map(fun mbta_line_sup:cast_entity_to_all/1, Entities).

fetch_mbta_alerts() ->
    MbtaData = fetch_mbta_alert_protobuf(),
    ParsedData = gtfs_realtime_pb:decode_feedmessage(MbtaData),
    ParsedData.

fetch_mbta_alert_protobuf() ->
    {ok, {{_HttpVer, _Code, _Msg}, _Headers, Body}} =
        httpc:request(get, {?MBTA_ALERT_URL, []}, [], []),
    erlang:iolist_to_binary(Body).

reset_heartbeat() ->
    Server = self(),
    spawn(fun() ->
        timer:apply_after(?HEARTBEAT_MILLIS, gen_server, cast, [Server, heartbeat])
    end).
