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
    long_gc/1,
    large_heap/1,
    busy_port/1,
    busy_dist_port/1
]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        telemetry_registry,
        long_message_queue,
        long_process_schedule,
        long_gc,
        large_heap,
        busy_port,
        busy_dist_port
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
    ?assertReceive({event, [large_heap], _, #{pid := Pid}}, 5000, "Haven't received event").

long_gc(_Config) ->
    Self = self(),
    with_monitor_option({long_gc, 10}, fun() ->
        _ = lists:seq(1, 12000000),
        erlang:garbage_collect(),
        ?assertReceive(
            {event, [long_gc], #{timeout := _}, #{pid := Self}},
            5000,
            "Haven't received event"
        )
    end).

%% A port whose program never reads its standard input fills up its queue
%% and suspends the process that writes to it.
busy_port(_Config) ->
    case os:find_executable("sleep") of
        false ->
            {skip, "No sleep executable"};
        Sleep ->
            Port = open_port({spawn_executable, Sleep}, [{args, ["15"]}]),
            Flooder = spawn(fun() -> flood_port(Port) end),
            try
                ?assertReceive(
                    {event, [busy_port], #{count := 1}, #{
                        suspended_pid := Flooder, port := Port
                    }},
                    5000,
                    "Haven't received event"
                )
            after
                exit(Flooder, kill),
                port_close(Port)
            end
    end.

%% The distribution port fills up when the other node stops reading its
%% socket. The peer node is stopped with SIGSTOP for the duration of the
%% case and resumed in the cleanup.
busy_dist_port(_Config) ->
    maybe
        ok ?= ensure_distribution(),
        {ok, Peer, Node} ?= start_peer(),
        stopped_peer_flood(Peer, Node)
    end.

stopped_peer_flood(Peer, Node) ->
    Remote = spawn(Node, fun() -> timer:sleep(60000) end),
    OsPid = lists:flatten(erpc:call(Node, os, getpid, [])),
    try
        [] = os:cmd("kill -STOP " ++ OsPid),
        Flooder = spawn(fun() -> flood_dist(Remote) end),
        try
            ?assertReceive(
                {event, [busy_dist_port], #{count := 1}, #{suspended_pid := Flooder, port := _}},
                5000,
                "Haven't received event"
            )
        after
            exit(Flooder, kill)
        end
    after
        _ = os:cmd("kill -CONT " ++ OsPid),
        peer:stop(Peer)
    end.

%% Internal functions

%% The system monitor is global for the VM, so the previous options are
%% restored afterwards.
with_monitor_option({Key, _} = Option, Fun) ->
    {Monitor, Options} = erlang:system_monitor(),
    _ = erlang:system_monitor(Monitor, [Option | lists:keydelete(Key, 1, Options)]),
    try
        Fun()
    after
        _ = erlang:system_monitor(Monitor, Options)
    end.

flood_port(Port) ->
    Data = binary:copy(<<0>>, 1048576),
    flood_port(Port, Data, 100).

flood_port(_Port, _Data, 0) ->
    ok;
flood_port(Port, Data, N) ->
    true = erlang:port_command(Port, Data),
    flood_port(Port, Data, N - 1).

flood_dist(Remote) ->
    Data = binary:copy(<<0>>, 1048576),
    flood_dist(Remote, Data, 500).

flood_dist(_Remote, _Data, 0) ->
    ok;
flood_dist(Remote, Data, N) ->
    Remote ! Data,
    flood_dist(Remote, Data, N - 1).

ensure_distribution() ->
    case net_kernel:start([sauron_suite, shortnames]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> {skip, {distribution, Reason}}
    end.

start_peer() ->
    try peer:start_link(#{name => sauron_suite_peer, args => ["-noinput"], wait_boot => 10000}) of
        {ok, Peer, Node} -> {ok, Peer, Node};
        Error -> {skip, {peer, Error}}
    catch
        Class:Reason -> {skip, {peer, Class, Reason}}
    end.
