% SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(sauron_app).

-moduledoc false.

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sauron_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
