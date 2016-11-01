-module(mbta_line).
-behaviour(gen_server).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").
-compile([{parse_transform, lager_transform}]).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(WATCHER_HEARTBEAT, 10000).
-define(TIMEOUT, 30000).

-record(state, {line, ets}).

start_link(Line) when is_binary(Line) ->
    start_link(erlang:binary_to_list(Line));
start_link(Line) when is_list(Line) ->
    gen_server:start_link(?MODULE, [Line], []).

init([Line]) ->
    lager:info("Started line alerter for the ~p line~n", [Line]),
    Ets = init_ets(),
    {ok, #state{ets=Ets, line=Line}}.

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


handle_feed_entity(#state{line=Line, ets=Ets}, FeedEntity) ->
    case feed_entity_acked(Ets, FeedEntity) of
        true -> ok;
        false ->
            ack_feedentity(Ets, FeedEntity),
            check_and_alert(Line, FeedEntity)
    end.

check_and_alert(Line, FeedEntity) ->
    Alert = FeedEntity#feedentity.alert,
    case alert_affects_route_id(Alert, Line) of
        false -> ok;
        true -> send_alert(Alert)
    end.

alert_affects_route_id(Alert, RouteId) ->
    Entities = Alert#alert.informed_entity,
    lists:any(
        fun(Entity) -> Entity#entityselector.route_id == RouteId end,
        Entities
    ).

send_alert(Alert) ->
    AlertText = mbta:pretty_print_alert(Alert),
    io:format(AlertText),
    ok.
