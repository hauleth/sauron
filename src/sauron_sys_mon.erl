% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

%% @hidden

-module(sauron_sys_mon).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

init(Opts) ->
    erlang:system_monitor(self(), Opts),
    {ok, []}.

handle_call(_Msg, _Ref, State) ->
    {reply, unimplemented, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({monitor, Subject, Type, Info}, State) ->
    execute(Type, Subject, Info),

    {noreply, State}.

% `long_gc` and `large_heap` have almost exactly the same format, so we can
% handle them together
execute(Type, Pid, Info) when Type =:= long_gc; Type =:= large_heap ->
    telemetry:execute(
        [erlang, sys_mon, Type],
        maps:from_list(Info),
        #{gc_pid => Pid}
    );
execute(long_schedule, Pid, Info) when is_pid(Pid) ->
    {timeout, Timeout} = lists:keyfind(timeout, 1, Info),
    {in, In} = lists:keyfind(in, 1, Info),
    {out, Out} = lists:keyfind(out, 1, Info),

    telemetry:execute(
        [erlang, sys_mon, long_schedule, process],
        #{timeout => Timeout},
        #{
            pid => Pid,
            in => In,
            out => Out
        }
    );
execute(long_schedule, Port, Info) ->
    {timeout, Timeout} = lists:keyfind(timeout, 1, Info),
    {port_op, Op} = lists:keyfind(port_op, 1, Info),

    telemetry:execute([erlang, sys_mon, long_schedule, port], #{timeout => Timeout}, #{
        port => Port,
        operation => Op
    });
% `busy_port` and `busy_port_dist` store exactly the same data, just with
% different meaning
execute(Type, Pid, Port) when Type =:= busy_port; Type =:= busy_dist_port ->
    telemetry:execute([erlang, sys_mon, Type], #{count => 1}, #{
        suspended_pid => Pid,
        port => Port
    });
% Process entered long_message_queue state
execute(long_message_queue, Pid, true) ->
    telemetry:execute([erlang, sys_mon, long_message_queue], #{count => 1}, #{pid => Pid});
% Process exited long_message_queue state
execute(long_message_queue, Pid, false) ->
    telemetry:execute([erlang, sys_mon, long_message_queue], #{count => -1}, #{pid => Pid}).
