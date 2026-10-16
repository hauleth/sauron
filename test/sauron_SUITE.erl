% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(sauron_SUITE).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    telemetry_registry/1,
    long_message_queue/1,
    long_process_schedule/1,
    large_heap/1
]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        telemetry_registry,
        long_message_queue,
        long_process_schedule,
        large_heap
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(sauron),
    telemetry_registry:discover_all(sauron),
    Config.
end_per_suite(Config) ->
    Config.

init_per_testcase(Name, Config) ->
    Events =
        #{
            Ev => sauron_telemetry_spec:compile(Meta)
         || {Ev, _, Meta} <- telemetry_registry:list_events()
        },
    telemetry:attach_many(
        {?MODULE, Name},
        maps:keys(Events),
        fun telemetry_callback/4,
        {self(), Events}
    ),
    Config.

end_per_testcase(Name, _Config) ->
    telemetry:detach({?MODULE, Name}).

telemetry_callback([erlang, sys_mon | EventName] = E, Measure, Meta, {Listener, Specs}) when
    is_pid(Listener)
->
    maybe
        #{E := {MeasSpec, MetaSpec}} ?= Specs,
        {true, _, _, _} ?=
            {sauron_telemetry_spec:validate(MeasSpec, Measure), measure, MeasSpec, Measure},
        {true, _, _, _} ?= {sauron_telemetry_spec:validate(MetaSpec, Meta), meta, MetaSpec, Meta},
        Listener ! {event, EventName, Measure, Meta}
    else
        {_, Type, Spec, Val} ->
            Listener !
                {ct_fail, "Event ~p ~p do not match spec:~n~nType: ~p~nValue: ~p", [
                    E, Type, Spec, Val
                ]}
    end.

-define(assertReceive(Message, Timeout, Log),
    Message = (fun() ->
        receive
            Message = __M -> __M;
            {ct_fail, FailMsg, FailArgs} -> ct:fail(FailMsg, FailArgs)
        after Timeout -> ct:fail(Log)
        end
    end)()
).

telemetry_registry(_Config) ->
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

long_process_schedule(_Config) ->
    _ = binary:copy(<<0>>, 50000000),
    Self = self(),
    ?assertReceive(
        {event, [long_schedule, process], #{timeout := _}, #{pid := Self}},
        5000,
        "Haven't received event"
    ).

large_heap(_Config) ->
    Pid = self(),
    _ = lists:seq(1, 10000000),
    erlang:garbage_collect(),
    ?assertReceive({event, [large_heap], _, #{gc_pid := Pid}}, 5000, "Haven't received event").
