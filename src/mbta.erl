-module(mbta).
-compile([{parse_transform, lager_transform}]).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").

-define(MBTA_ALERT_URL, "http://developer.mbta.com/lib/GTRTFS/Alerts/Alerts.pb").

%% Application callbacks
-export([
    start_line/2
]).

start_line(Line, Color) ->
    supervisor:start_child(mbta_line_sup, [Line, Color]).
