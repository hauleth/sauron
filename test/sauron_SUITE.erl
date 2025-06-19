-module(sauron_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        telemetry_registry,
        long_message_queue,
        large_heap
    ].

init_per_testcase(_Name, Config) ->
    {ok, _} = application:ensure_all_started(sauron),
    Config.

end_per_testcase(Name, _Config) ->
    telemetry:detach({?MODULE, Name}).

-define(capture(Suffix), capture(?FUNCTION_NAME, Suffix)).

-define(assertReceive(Message, Timeout, Log),
    receive
        Message -> ok
    after Timeout -> ct:fail(Log)
    end
).

telemetry_registry(_Config) ->
    telemetry_registry:discover_all(sauron),
    Events = telemetry_registry:list_events(),

    ?assertMatch({_, _, _}, find_event([erlang, sys_mon, long_gc], Events)),
    ?assertMatch({_, _, _}, find_event([erlang, sys_mon, large_heap], Events)),
    ?assertMatch({_, _, _}, find_event([erlang, sys_mon, long_schedule, process], Events)),
    ?assertMatch({_, _, _}, find_event([erlang, sys_mon, long_schedule, port], Events)),
    ?assertMatch({_, _, _}, find_event([erlang, sys_mon, long_message_queue], Events)),
    ?assertMatch({_, _, _}, find_event([erlang, sys_mon, busy_port], Events)),
    ?assertMatch({_, _, _}, find_event([erlang, sys_mon, busy_dist_port], Events)).

find_event(Name, Events) -> lists:keyfind(Name, 1, Events).

long_message_queue(_Config) ->
    ?capture([long_message_queue]),
    Pid = spawn(fun() -> message_loop(collect) end),
    [Pid ! N || N <- lists:seq(1, 1000)],
    ?assertReceive(
        {event, [long_message_queue], #{count := 1}, #{pid := Pid}},
        5000,
        "Haven't received up event"
    ),
    Pid ! drain,
    ?assertReceive(
        {event, [long_message_queue], #{count := -1}, #{pid := Pid}},
        5000,
        "Haven't received down event"
    ),
    Pid ! finish.

message_loop(collect) ->
    receive
        drain -> message_loop(drain)
    end;
message_loop(drain) ->
    receive
        N when is_integer(N) -> message_loop(drain);
        finish -> ok
    end.

large_heap(_Config) ->
    ?capture([large_heap]),
    Pid = self(),
    _ = lists:seq(1, 1000000),
    erlang:garbage_collect(),
    ?assertReceive({event, [large_heap], _, #{gc_pid := Pid}}, 5000, "Haven't received event").

acc_loop(Acc) ->
    receive
        finish -> ok;
        reset -> acc_loop([]);
        Num when is_integer(Num) -> acc_loop([Num | Acc])
    end.

capture(Name, EventSuffix) ->
    telemetry:attach(
        Name,
        [erlang, sys_mon] ++ EventSuffix,
        fun([erlang, sys_mon | EventName], Measure, Meta, Listener) ->
            Listener ! {event, EventName, Measure, Meta}
        end,
        self()
    ).
