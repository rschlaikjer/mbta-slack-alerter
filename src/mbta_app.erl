-module(mbta_app).
-compile([{parse_transform, lager_transform}]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = mbta_sup:start_link(),
    {ok, LinesToStart} = application:get_env(mbta, watched_lines),
    lists:map(
        fun({Line, Color}) -> mbta:start_line(Line, Color) end,
        LinesToStart
    ),
    {ok, Pid}.

stop(_State) ->
    ok.

