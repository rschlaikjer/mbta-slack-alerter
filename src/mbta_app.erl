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
    {ok, _} = mbta_sup:start_link().

stop(_State) ->
    ok.

