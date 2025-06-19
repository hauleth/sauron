%% @hidden

-module(sauron).

-export([]).

-telemetry_event(#{
    event => [erlang, sys_mon, long_gc],
    description => <<"Long garbage collection runs">>,
    measurements =>
        <<
            "#{\n"
            "  timeout => pos_integer(),\n"
            "  heap_size => pos_integer(),\n"
            "  heap_block_size => pos_integer(),\n"
            "  stack_size => pos_integer(),\n"
            "  mbuf_size => pos_integer(),\n"
            "  old_heap_size => pos_integer()\n"
            "}"
        >>,
    metadata => <<"#{pid => pid()}">>
}).
-telemetry_event(#{
    event => [erlang, sys_mon, large_heap],
    description => <<"Large heap allocation after garbage collection">>,
    measurements =>
        <<
            "#{\n"
            "  heap_size => pos_integer(),\n"
            "  heap_block_size => pos_integer(),\n"
            "  stack_size => pos_integer(),\n"
            "  mbuf_size => pos_integer(),\n"
            "  old_heap_size => pos_integer()\n"
            "}"
        >>,
    metadata => <<"#{pid => pid()}">>
}).

-telemetry_event(#{
    event => [erlang, sys_mon, long_schedule, process],
    description => <<"Process that occupied scheduler for a long time">>,
    measurements =>
        <<
            "#{\n"
            "  timeout => pos_integer()\n"
            "}"
        >>,
    metadata =>
        <<
            "#{\n"
            "  pid => pid(),\n"
            "  in => undefined | mfa(),\n"
            "  out => undefined | mfa()\n"
            "}"
        >>
}).

-telemetry_event(#{
    event => [erlang, sys_mon, long_schedule, port],
    description => <<"Port that occupied scheduler for a long time">>,
    measurements =>
        <<
            "#{\n"
            "  timeout => pos_integer()\n"
            "}"
        >>,
    metadata =>
        <<
            "#{\n"
            "  port => port(),\n"
            "  operation => atom()\n"
            "}"
        >>
}).

-telemetry_event(#{
    event => [erlang, sys_mon, long_message_queue],
    description => <<"Process with large message queue">>,
    measurements =>
        <<
            "#{\n"
            "  count => integer()\n"
            "}"
        >>,
    metadata =>
        <<
            "#{\n"
            "  port => port()\n"
            "}"
        >>
}).

-telemetry_event(#{
    event => [erlang, sys_mon, busy_port],
    description => <<"">>,
    measurements =>
        <<
            "#{\n"
            "  count => pos_integer()\n"
            "}"
        >>,
    metadata =>
        <<
            "#{\n"
            "  suspended_pid => pid(),\n"
            "  port => port()\n"
            "}"
        >>
}).
-telemetry_event(#{
    event => [erlang, sys_mon, busy_dist_port],
    description => <<"">>,
    measurements =>
        <<
            "#{\n"
            "  count => pos_integer()\n"
            "}"
        >>,
    metadata =>
        <<
            "#{\n"
            "  suspended_pid => pid(),\n"
            "  port => port()\n"
            "}"
        >>
}).
