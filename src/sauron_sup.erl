% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

%%%-------------------------------------------------------------------
%% @hidden
%% sauron top level supervisor.
%%%-------------------------------------------------------------------

-module(sauron_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    Opts = load_options(),
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => sauron_sys_mon,
            start => {sauron_sys_mon, start_link, [Opts]},
            modules => [sauron_sys_mon]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

load_options() ->
    {ok, Opts} = application:get_env(sauron, monitor),
    do_opts(Opts).

do_opts([]) ->
    [];
do_opts([{_, false} | Rest]) ->
    do_opts(Rest);
do_opts([{Name, true} | Rest]) ->
    [Name | do_opts(Rest)];
do_opts([{long_message_queue, {Disable, Enable}} = Opt | Rest]) when
    is_integer(Disable), is_integer(Enable), Disable >= 0, Disable < Enable
->
    [Opt | do_opts(Rest)];
do_opts([{_Name, Val} = Opt | Rest]) when is_integer(Val) ->
    [Opt | do_opts(Rest)];
do_opts([Invalid | _]) ->
    error({invalid_option, Invalid}).
