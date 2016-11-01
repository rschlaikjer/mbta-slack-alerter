-module(mbta).
-compile([{parse_transform, lager_transform}]).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").

-define(MBTA_ALERT_URL, "http://developer.mbta.com/lib/GTRTFS/Alerts/Alerts.pb").

%% Application callbacks
-export([
    compile_protobuf/0,
    start_line/2
]).

compile_protobuf() ->
    protobuffs_compile:scan_file(
        filename:join([
            code:priv_dir(mbta),
            "gtfs_realtime.proto"
        ])
    ).

start_line(Line, Color) ->
    supervisor:start_child(mbta_line_sup, [Line, Color]).
