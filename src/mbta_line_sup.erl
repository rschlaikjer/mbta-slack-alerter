-module(mbta_line_sup).
-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").

-export([start_link/0]).
-export([init/1]).
-export([cast_entity_to_all/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

cast_entity_to_all(Entity=#feedentity{}) ->
    ChildPids = [Pid || {_, Pid, _, _} <- supervisor:which_children(mbta_line_sup)],
    lager:info("Casting entity to ~p~n", [ChildPids]),
    lists:map(
        fun(ChildPid) -> gen_server:cast(ChildPid, {entity, Entity}) end,
        ChildPids
    ).

child_spec() -> {
  mbta_line,
  {mbta_line, start_link, []},
  transient,
  3000,
  worker,
  [mbta_line]
}.

init([]) ->
  {ok, {{simple_one_for_one, 10, 10}, [child_spec()]}}.


