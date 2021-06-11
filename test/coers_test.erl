-module(coers_test).

-include_lib("results/include/results.hrl").
-include_lib("eunit/include/eunit.hrl").

value_test() ->
    ?assertEqual("a", coers:value(results:new("a"))).

error_test() ->
    ?assertEqual(oops, coers:error(results:new_error(oops))).

has_error_test() ->
    ?assert(coers:has_error(results:new_error(oops))),
    %% LFE-friendly alias
    ?assert(coers:'error?'(results:new_error(oops))).

to_string_test() ->
    ?assertEqual("coers", coers:value(coers:to_string("coers"))),
    ?assertEqual("coers", coers:value(coers:to_string(coers))),
    ?assertEqual("5/16", coers:value(coers:to_string("5/16"))),
    ?assertEqual("", coers:value(coers:to_string([]))),
    ?assertEqual("42", coers:value(coers:to_string(42))),
    ?assertEqual("coers", coers:value(coers:to_string(<<"coers">>))),
    ?assertEqual("42.0", string:substr(coers:value(coers:to_string(42.0)), 1, 4)),
    %% LFE-friendly alias
    ?assertEqual("42.0", coers:value(coers:'->string'(42.0))).

to_term_atomic_test() ->
    R = coers:to_term("an_atom"),
    ?assertEqual(an_atom, coers:value(R)).

to_term_list_test() ->
    R = coers:to_term("[1,2,3,4]"),
    ?assertEqual([1,2,3,4], coers:value(R)).

to_term_numeric_test() ->
    R = coers:to_term("{45, 45.3}"),
    ?assertEqual({45, 45.3}, coers:value(R)).

to_term_bitstring_test() ->
    R = coers:to_term("<<\"foo\">>"),
    ?assertEqual( <<"foo">>, coers:value(R)).

to_term_errors_test() ->
    ?assertEqual({erl_parse,"Could not convert string \"{45\""}, coers:error(coers:to_term("{45"))).

to_term_defaults_test() ->
    R = coers:to_term("<<43", "<<\"42\">>"),
    U = coers:to_term("foo", "bar"),
    ?assertEqual(<<"42">>, coers:value(R)),
    ?assertEqual(foo, coers:value(U)),
    %% LFE-friendly alias
    ?assertEqual(foo, coers:value(coers:'->term'("foo", "bar"))).

to_int_test() ->
    ?assertEqual(1000, coers:value(coers:to_int("1000"))),
    ?assertEqual(444, coers:value(coers:to_int(444))),
    ?assertEqual(123, coers:value(coers:to_int('123'))),
    ?assertEqual(123, coers:value(coers:to_int("+0123"))),
    ?assertEqual(-123, coers:value(coers:to_int("-0123"))),
    ?assertEqual(23, coers:value(coers:to_int(<<"23">>))),
    %% LFE-friendly alias
    ?assertEqual(23, coers:value(coers:'->int'(<<"23">>))).

to_int_errors_test() ->
    ?assertEqual({error,"Could not convert 111.2 to int"}, coers:error(coers:to_int(111.2))),
    ?assertEqual({badarg,"Could not convert \"5/16\" (type rational) to int"}, coers:error(coers:to_int("5/16"))),
    ?assertEqual({badarg,"Could not convert \"-0/16\" (type rational) to int"}, coers:error(coers:to_int("-0/16"))),
    ?assertEqual({badarg,"Could not convert \"+5/0\" (type rational) to int"}, coers:error(coers:to_int("+5/0"))),
    %% LFE-friendly alias
    ?assertEqual({badarg,"Could not convert \"+5/0\" (type rational) to int"}, coers:error(coers:'->int'("+5/0"))).

to_int_defaults_test() ->
    ?assertEqual(111, coers:value(coers:to_int("foo", 111))),
    %% LFE-friendly alias
    ?assertEqual(111, coers:value(coers:'->int'("foo", 111))).

to_float_test() ->
    ?assertEqual(123.23, coers:value(coers:to_float('123.23'))),
    ?assertEqual(111.2, coers:value(coers:to_float(111.2))),
    ?assertEqual(123.7654, coers:value(coers:to_float("+0123.7654"))),
    ?assertEqual(-123.7654, coers:value(coers:to_float("-0123.7654"))),
    ?assertEqual(23.78, coers:value(coers:to_float(<<"23.78">>))),
    %% LFE-friendly alias
    ?assertEqual(23.78, coers:value(coers:'->float'(<<"23.78">>))).

to_float_errors_test() ->
    ?assertEqual({error,"Could not convert 1000 to float"}, coers:error(coers:to_float("1000"))),
    ?assertEqual({error,"Could not convert 444 to float"}, coers:error(coers:to_float(444))),
    %% LFE-friendly alias
    ?assertEqual({error,"Could not convert 444 to float"}, coers:error(coers:'->float'(444))).

to_float_defaults_test() ->
    ?assertEqual(444.0, coers:value(coers:to_float("444", "444.0"))),
    ?assertEqual(444.0, coers:value(coers:to_float(444, '444.0'))),
    %% LFE-friendly alias
    ?assertEqual(444.0, coers:value(coers:'->float'(444, '444.0'))).

to_atom_test() ->
    ?assertEqual(foo, coers:value(coers:to_atom(foo))),
    ?assertEqual(foo, coers:value(coers:to_atom("foo"))),
    ?assertEqual(foo, coers:value(coers:to_atom(<<"foo">>))),
    ?assertEqual('222.987', coers:value(coers:to_atom(222.987))),
    %% LFE-friendly alias
    ?assertEqual('222.987', coers:value(coers:'->atom'(222.987))).

to_bool_test() ->
    ?assert(coers:value(coers:to_bool("true"))),
    ?assert(coers:value(coers:to_bool(true))),
    ?assert(coers:value(coers:to_bool(1))),
    ?assert(coers:value(coers:to_bool(1.0))),
    ?assert(coers:value(coers:to_bool("TrUe"))),
    ?assert(coers:value(coers:to_bool(trUe))),
    ?assertNot(coers:value(coers:to_bool(false))),
    ?assertNot(coers:value(coers:to_bool("False"))),
    ?assertNot(coers:value(coers:to_bool(<<"false">>))),
    ?assertNot(coers:value(coers:to_bool(0))),
    ?assertNot(coers:value(coers:to_bool(0.0))),
    %% LFE-friendly alias
    ?assert(coers:value(coers:'->bool'("true"))).

get_type_test() ->
    ?assertEqual(integer, coers:get_type("516")),
    ?assertEqual(integer, coers:get_type("-516")),
    ?assertEqual(integer, coers:get_type("+516")),
    ?assertEqual(float, coers:get_type("5.16")),
    ?assertEqual(float, coers:get_type("-5.16")),
    ?assertEqual(float, coers:get_type("+5.16")),
    ?assertEqual(float, coers:get_type("5.16e+42")),
    ?assertEqual(float, coers:get_type("5.16e-42")),
    ?assertEqual(float, coers:get_type("-5.16e-42")),
    ?assertEqual(float, coers:get_type("+5.16e-42")),
    ?assertEqual(rational, coers:get_type("5/16")),
    ?assertEqual(rational, coers:get_type("-5/16")),
    ?assertEqual(rational, coers:get_type("+5/16")),
    ?assertEqual(any, coers:get_type("1.2.3")),
    ?assertEqual(any, coers:get_type("1.2.3-rc4")),
    %% LFE-friendly alias
    ?assertEqual(integer, coers:'get-type'("516")).

to_rational_test() ->
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational("5/16")),
    ?assertEqual({result,{fraction,-5,16},undefined}, coers:to_rational("-5/16")),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational(<<"5/16">>)),
    ?assertEqual({result,{fraction,-5,16},undefined}, coers:to_rational(<<"-5/16">>)),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational({5,16})),
    ?assertEqual({result,{fraction,-5,16},undefined}, coers:to_rational({-5,16})),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational({"5","16"})),
    %% LFE-friendly alias
    ?assertEqual({result,{fraction,5,16},undefined}, coers:'->rational'({"5","16"})).

to_rational_errors_test() ->
    ?assertEqual({badarg,"Could not convert \"5/16\" (type rational) to float"}, coers:error(coers:to_float("5/16"))),
    ?assertEqual({badarg,"Could not convert \"-0/16\" (type rational) to float"}, coers:error(coers:to_float("-0/16"))),
    ?assertEqual({badarg,"Could not convert \"+5/0\" (type rational) to float"}, coers:error(coers:to_float("+5/0"))),
    ?assertEqual({nomatch,"Could not convert \"0.3125\" to ratio"},
                 coers:error(coers:to_rational("0.3125"))).

to_rational_defaults_test() ->
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational("0.3125", "5/16")),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational("bob", "5/16")),
    %% LFE-friendly alias
    ?assertEqual({result,{fraction,5,16},undefined}, coers:'->rational'("bob", "5/16")).
