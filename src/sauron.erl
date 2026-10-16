% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(sauron).

-moduledoc false.

-export([]).

-telemetry_event(#{
    event => [erlang, sys_mon, long_gc],
    description => ~"Long garbage collection runs",
    measurements =>
        ~"""
        #{
          timeout => pos_integer(),
          heap_size => pos_integer(),
          heap_block_size => pos_integer(),
          stack_size => pos_integer(),
          mbuf_size => non_neg_integer(),
          old_heap_size => pos_integer(),
          old_heap_block_size => pos_integer()
        }
        """,
    metadata => ~"#{pid => pid()}"
}).
-telemetry_event(#{
    event => [erlang, sys_mon, large_heap],
    description => ~"Large heap allocation after garbage collection",
    measurements =>
        ~"""
        #{
          heap_size => pos_integer(),
          heap_block_size => pos_integer(),
          stack_size => pos_integer(),
          mbuf_size => non_neg_integer(),
          old_heap_size => pos_integer(),
          old_heap_block_size => pos_integer()
        }
        """,
    metadata => ~"#{gc_pid => pid()}"
}).

-telemetry_event(#{
    event => [erlang, sys_mon, long_schedule, process],
    description => ~"Process that occupied scheduler for a long time",
    measurements =>
        ~"""
        #{
          timeout => pos_integer()
        }
        """,
    metadata =>
        ~"""
        #{
          pid => pid(),
          in => undefined | mfa(),
          out => undefined | mfa()
        }
        """
}).

-telemetry_event(#{
    event => [erlang, sys_mon, long_schedule, port],
    description => ~"Port that occupied scheduler for a long time",
    measurements => ~"#{timeout => pos_integer()}",
    metadata =>
        ~"""
        #{
          port => port(),
          operation => atom()
        }
        """
}).

-telemetry_event(#{
    event => [erlang, sys_mon, long_message_queue],
    description => ~"Process with large message queue",
    measurements => ~"#{count => integer()}",
    metadata => ~"#{pid => pid()}"
}).

-telemetry_event(#{
    event => [erlang, sys_mon, busy_port],
    description => ~"",
    measurements => ~"#{count => integer()}",
    metadata =>
        ~"""
        #{
          suspended_pid => pid(),
          port => port()
        }
        """
}).
-telemetry_event(#{
    event => [erlang, sys_mon, busy_dist_port],
    description => ~"",
    measurements => ~"#{count => integer()}",
    metadata =>
        ~"""
        #{
          suspended_pid => pid(),
          port => port()
        }
        """
}).
