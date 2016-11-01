-module(mbta_app).
-compile([{parse_transform, lager_transform}]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    mbta:compile_protobuf(),
    {ok, Pid} = mbta_sup:start_link(),
    mbta:start_line("Red", <<"#E23139">>),
    mbta:start_line("Green-B", <<"#019362">>),
    mbta:start_line("Green-C", <<"#019362">>),
    mbta:start_line("Green-D", <<"#019362">>),
    mbta:start_line("Green-E", <<"#019362">>),
    mbta:start_line("Blue", <<"#007FC5">>),
    mbta:start_line("Orange", <<"#F7941D">>),
    {ok, Pid}.

stop(_State) ->
    ok.

