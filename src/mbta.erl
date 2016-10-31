-module(mbta).
-compile([{parse_transform, lager_transform}]).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").

%% Application callbacks
-export([
    fetch_mbta_alerts/0,
    compile_protobuf/0,
    fetch_alerts_for_line/1,
    pretty_print_alert/1
]).

-define(MBTA_ALERT_URL, "http://developer.mbta.com/lib/GTRTFS/Alerts/Alerts.pb").

compile_protobuf() ->
    protobuffs_compile:scan_file(
        filename:join([
            code:priv_dir(mbta),
            "gtfs_realtime.proto"
        ])
    ).

fetch_mbta_alerts() ->
    MbtaData = fetch_mbta_alert_protobuf(),
    ParsedData = gtfs_realtime_pb:decode_feedmessage(MbtaData),
    ParsedData.

fetch_alerts_for_line(Line) ->
    fetch_alerts_for_line(Line, fetch_mbta_alerts()).

fetch_alerts_for_line(Line, FeedMessage) ->
    Entities = FeedMessage#feedmessage.entity,
    Alerts = [ Entity#feedentity.alert || Entity <- Entities ],
    Relevant = lists:filter(
        fun(Alert) -> alert_affects_route_id(Alert, Line) end,
        Alerts
    ),
    Relevant.

alert_affects_route_id(Alert, RouteId) ->
    Entities = Alert#alert.informed_entity,
    lists:any(
        fun(Entity) -> Entity#entityselector.route_id == RouteId end,
        Entities
    ).

get_translation(#translatedstring{translation=Translations}, Language) ->
    case lists:filter(
        fun(Translation) -> Translation#translatedstring_translation.language == Language end,
        Translations
    ) of
        [Translation] -> Translation#translatedstring_translation.text;
        _ -> undefined
    end.

pretty_print_alert(Alert)->
    Header = Alert#alert.header_text,
    Description = Alert#alert.description_text,
    AlertText = io_lib:format(
        "Alert! ~s. Cause: ~s.~n~s~n",
        [
            get_translation(Header, "en"),
            Alert#alert.cause,
            get_translation(Description, "en")
        ]
    ),
    io:format(AlertText),
    AlertText.


fetch_mbta_alert_protobuf() ->
    {ok, {{HttpVer, Code, Msg}, Headers, Body}} =
        httpc:request(get, {?MBTA_ALERT_URL, []}, [], []),
    erlang:iolist_to_binary(Body).

