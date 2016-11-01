-module(mbta_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    Children = [
    {
        mbta_line_sup,
        {mbta_line_sup, start_link, []},
        permanent,
        3000,
        supervisor,
        [mbta_line_sup]
    }
    ],
    {ok, { {one_for_one, 10, 10}, Children } }.

