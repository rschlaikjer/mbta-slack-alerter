-module(mbta_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    Children = [
    ],
    {ok, { {one_for_one, 10, 10}, Children } }.

