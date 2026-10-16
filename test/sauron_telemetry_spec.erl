% SPDX-FileCopyrightText: 2026 Łukasz Niemier <~@hauleth.dev>
%
% SPDX-License-Identifier: Apache-2.0

-module(sauron_telemetry_spec).

-export([compile/1, validate/2]).

compile(#{measurements := Meas, metadata := Meta}) ->
    {compile_type(Meas), compile_type(Meta)}.

compile_type(Bin) ->
    Code = "-type schema() :: " ++ binary_to_list(Bin) ++ ".",
    {ok, Tokens, _} = erl_scan:string(Code),
    {ok, {attribute, _, type, {schema, TypeAst, []}}} =
        erl_parse:parse_form(Tokens),
    build_spec(TypeAst).

build_spec({type, _, map, Fields}) ->
    {map, lists:keysort(1, [build_field(Field) || Field <- Fields])};
build_spec({atom, _, Name}) ->
    {literal, Name};
build_spec({type, _, Name, []}) ->
    {type, Name};
build_spec({type, _, union, Types}) ->
    {union, [build_spec(Type) || Type <- Types]}.

build_field({type, _, map_field_assoc, [{atom, _, Key}, Type]}) ->
    {Key, build_spec(Type)}.

validate({map, Spec}, Map) when is_map(Map) ->
    MapList = lists:keysort(1, maps:to_list(Map)),
    do_validate_map(Spec, MapList);
validate({map, _Spec}, _Val) ->
    false;
validate({literal, Spec}, Val) ->
    Spec =:= Val;
validate({type, pos_integer}, Val) ->
    is_integer(Val) andalso Val > 0;
validate({type, non_neg_integer}, Val) ->
    is_integer(Val) andalso Val >= 0;
validate({type, integer}, Val) ->
    is_integer(Val);
validate({type, pid}, Val) ->
    is_pid(Val);
validate({type, atom}, Val) ->
    is_atom(Val);
validate({type, port}, Val) ->
    is_port(Val);
validate({type, mfa}, {M, F, A}) ->
    is_atom(M) andalso is_atom(F) andalso is_integer(A) andalso A >= 0;
validate({type, mfa}, _Val) ->
    false;
validate({union, Types}, Val) ->
    lists:any(fun(T) -> validate(T, Val) end, Types).

do_validate_map([], []) ->
    true;
do_validate_map([{K, Type} | Spec], [{K, Val} | Map]) ->
    validate(Type, Val) andalso do_validate_map(Spec, Map);
do_validate_map(_, _) ->
    false.
