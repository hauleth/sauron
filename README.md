<!--
SPDX-FileCopyrightText: 2025 Łukasz Niemier <~@hauleth.dev>

SPDX-License-Identifier: Apache-2.0
-->

Sauron
=====

> There he took up again his great Ring in Barad-dur, and dwelt there, dark and
> silent, until he wrought himself a new guise, an image of malice and hatred
> made visible; and the Eye of Sauron the Terrible few could endure.
>
> -- *Silmarillion* - J. R. R. Tolkien

Process that translates Erlang system monitoring messages to Telemetry events
that can be then ingested by different components.

## Disclaimer

There can be only one process that is monitoring system at given time, this
application uses `telemetry` to allow multiple different systems and components
to watch for such system events.

## Overload protection

Currently there is no overload protection, so users must be cautious to the fact
that processing events should be fast. If there is need for longer processing
then it would be best to offload it to separate process.

## Events

All events are discoverable via `telemetry_registry` but the currently supported
list is:

* `erlang.sys_mon.long_gc`:
    + measurements:
        - `timeout`
        - `heap_size`
        - `heap_block_size`
        - `stack_size`
        - `mbuf_size`
        - `old_heap_size`
    + metadata:
        - `pid`
* `erlang.sys_mon.large_heap`:
    + measurements:
        - `heap_size`
        - `heap_block_size`
        - `stack_size`
        - `mbuf_size`
        - `old_heap_size`
    + metadata:
        - `pid`
* `erlang.sys_mon.long_schedule.process`:
    + measurements:
        - `timeout`
    + metadata:
        - `pid`
        - `in`
        - `out`
* `erlang.sys_mon.long_schedule.port`:
    + measurements:
        - `timeout`
    + metadata:
        - `port`
        - `operation`
* `erlang.sys_monitor.long_message_queue`:
    + measurements:
        - `count`
    + metadata:
        - `pid`
* `erlang.sys_monitor.busy_port`:
    + measurement:
        - `count`
    + metadata:
        - `suspended_pid`
        - `port`
* `erlang.sys_monitor.busy_dist_port`:
    + measurement:
        - `count`
    + metadata:
        - `suspended_pid`
        - `port`

For details of these values check out `trace:system/3` documentation in OTP.

## License

Apache-2.0
