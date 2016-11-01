-module(mbta).
-compile([{parse_transform, lager_transform}]).
-include_lib("mbta/include/gtfs_realtime_pb.hrl").

-define(MBTA_ALERT_URL, "http://developer.mbta.com/lib/GTRTFS/Alerts/Alerts.pb").

%% Application callbacks
-export([
    compile_protobuf/0,
    start_line/1,
    pretty_print_alert/1
]).

compile_protobuf() ->
    protobuffs_compile:scan_file(
        filename:join([
            code:priv_dir(mbta),
            "gtfs_realtime.proto"
        ])
    ).

start_line(Line) ->
    supervisor:start_child(mbta_line_sup, [Line]).

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
    Timerange = hd(Alert#alert.active_period),
    lager:info("Timerange: ~p~n", [Timerange]),
    AlertText = io_lib:format(
        "Alert! ~s until ~s. Cause: ~s.~n~s~n",
        [
            get_translation(Header, "en"),
            format_time(unix_seconds_to_datetime(Timerange#timerange.'end')),
            Alert#alert.cause,
            get_translation(Description, "en")
        ]
    ),
    io:format(AlertText),
    AlertText.


unix_seconds_to_datetime(Seconds) ->
    calendar:gregorian_seconds_to_datetime(Seconds + 62167219200).

format_time({{Y, M, D}, {H, Mi, S}}) ->
    io_lib:format("~2..0b:~2..0b:~2..0b ~b/~2..0b/~2..0b", [H, Mi, S, Y, M, D]).
