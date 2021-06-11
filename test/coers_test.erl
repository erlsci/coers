-module(coers_test).

-include_lib("results/include/results.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test for coers:is_ascii_char
is_ascii_char_test() ->
    Flag = lists:all(
             fun coers:is_ascii_char/1,
             lists:seq(32, 126)
            ),
    ?assert(Flag),
    ?assert(coers:is_ascii_char("A")),
    ?assertNot(coers:is_ascii_char(222)),
    ?assertNot(coers:is_ascii_char(a)).

%% Test for maybe_string
maybe_string_test() ->
    ?assert(coers:maybe_string("")),
    ?assert(coers:maybe_string("Hello")),
    ?assert(coers:maybe_string([32, 33, 34])),
    ?assertNot(coers:maybe_string(42)),
    ?assertNot(coers:maybe_string([0,1])),
    ?assertNot(coers:maybe_string(atom)),
    ?assertNot(coers:maybe_string(42.0)).

%% Test for to_string coersion
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

%% Test suits for magic coersion
of_string_atomic_test() ->
    R = coers:of_string("an_atom"),
    ?assertEqual(an_atom, coers:value(R)).

of_string_list_test() ->
    R = coers:of_string("[1,2,3,4]"),
    ?assertEqual([1,2,3,4], coers:value(R)).

of_string_numeric_test() ->
    R = coers:of_string("{45, 45.3}"),
    ?assertEqual({45, 45.3}, coers:value(R)).

of_string_bitstring_test() ->
    R = coers:of_string("<<\"foo\">>"),
    ?assertEqual( <<"foo">>, coers:value(R)).

of_string_errors_test() ->
    ?assertEqual({erl_parse,"Could not convert string \"{45\""}, coers:error(coers:of_string("{45"))).

of_string_defaults_test() ->
    R = coers:of_string("<<43", "<<\"42\">>"),
    U = coers:of_string("foo", "bar"),
    ?assertEqual(<<"42">>, coers:value(R)),
    ?assertEqual(foo, coers:value(U)).

to_int_test() ->
    F = fun(X) -> coers:value(coers:to_int(X)) end,
    G = fun(X, Y) -> coers:value(coers:to_int(X, Y)) end,
    ?assertEqual(1000, F("1000")),
    ?assertEqual(444, F(444)),
    ?assertEqual(123, F('123')),
    ?assertEqual(123, F("+0123")),
    ?assertEqual(-123, F("-0123")),
    ?assertEqual(23, F(<<"23">>)).

to_int_errors_test() ->
    ?assertEqual({error,"Could not convert 111.2 to int"}, coers:error(coers:to_int(111.2))),
    ?assertEqual({badarg,"Could not convert \"5/16\" (type rational) to int"}, coers:error(coers:to_int("5/16"))),
    ?assertEqual({badarg,"Could not convert \"-0/16\" (type rational) to int"}, coers:error(coers:to_int("-0/16"))),
    ?assertEqual({badarg,"Could not convert \"+5/0\" (type rational) to int"}, coers:error(coers:to_int("+5/0"))).

to_int_defaults_test() ->
    ?assertEqual(111, coers:value(coers:to_int("foo", 111))).

to_float_test() ->
    ?assertEqual(123.23, coers:value(coers:to_float('123.23'))),
    ?assertEqual(111.2, coers:value(coers:to_float(111.2))),
    ?assertEqual(123.7654, coers:value(coers:to_float("+0123.7654"))),
    ?assertEqual(-123.7654, coers:value(coers:to_float("-0123.7654"))),
    ?assertEqual(23.78, coers:value(coers:to_float(<<"23.78">>))).

to_float_errors_test() ->
    ?assertEqual({error,"Could not convert 1000 to flost"}, coers:error(coers:to_float("1000"))),
    ?assertEqual({error,"Could not convert 444 to flost"}, coers:error(coers:to_float(444))).

to_float_defaults_test() ->
    ?assertEqual(444.0, coers:value(coers:to_float("444", "444.0"))),
    ?assertEqual(444.0, coers:value(coers:to_float(444, '444.0'))).

to_atom_test() ->
    F = fun(X) -> coers:value(coers:to_atom(X)) end,
    ?assertEqual(foo, F(foo)),
    ?assertEqual(foo, F("foo")),
    ?assertEqual(foo, F(<<"foo">>)),
    ?assertEqual('222.987', F(222.987)).

to_bool_test() ->
    F = fun(X) -> coers:value(coers:to_bool(X)) end,
    ?assert(F("true")),
    ?assert(F(true)),
    ?assert(F(1)),
    ?assert(F(1.0)),
    ?assert(F("TrUe")),
    ?assert(F(trUe)),
    ?assertNot(F(false)),
    ?assertNot(F("False")),
    ?assertNot(F(<<"false">>)),
    ?assertNot(F(0)),
    ?assertNot(F(0.0)).

numeric_align_test() ->
    ?assertEqual(integer, coers:numeric_align("516")),
    ?assertEqual(integer, coers:numeric_align("-516")),
    ?assertEqual(integer, coers:numeric_align("+516")),
    ?assertEqual(float, coers:numeric_align("5.16")),
    ?assertEqual(float, coers:numeric_align("-5.16")),
    ?assertEqual(float, coers:numeric_align("+5.16")),
    ?assertEqual(float, coers:numeric_align("5.16e+42")),
    ?assertEqual(float, coers:numeric_align("5.16e-42")),
    ?assertEqual(float, coers:numeric_align("-5.16e-42")),
    ?assertEqual(float, coers:numeric_align("+5.16e-42")),
    ?assertEqual(rational, coers:numeric_align("5/16")),
    ?assertEqual(rational, coers:numeric_align("-5/16")),
    ?assertEqual(rational, coers:numeric_align("+5/16")),
    ?assertEqual(any, coers:numeric_align("1.2.3")),
    ?assertEqual(any, coers:numeric_align("1.2.3-rc4")).

to_rational_test() ->
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational("5/16")),
    ?assertEqual({result,{fraction,-5,16},undefined}, coers:to_rational("-5/16")),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational(<<"5/16">>)),
    ?assertEqual({result,{fraction,-5,16},undefined}, coers:to_rational(<<"-5/16">>)),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational({5,16})),
    ?assertEqual({result,{fraction,-5,16},undefined}, coers:to_rational({-5,16})),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational({"5","16"})).

to_rational_errors_test() ->
    ?assertEqual({badarg,"Could not convert \"5/16\" (type rational) to float"}, coers:error(coers:to_float("5/16"))),
    ?assertEqual({badarg,"Could not convert \"-0/16\" (type rational) to float"}, coers:error(coers:to_float("-0/16"))),
    ?assertEqual({badarg,"Could not convert \"+5/0\" (type rational) to float"}, coers:error(coers:to_float("+5/0"))),
    ?assertEqual({nomatch,"Could not convert \"0.3125\" to ratio"},
                 coers:error(coers:to_rational("0.3125"))).

to_rational_defaults_test() ->
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational("0.3125", "5/16")),
    ?assertEqual({result,{fraction,5,16},undefined}, coers:to_rational("bob", "5/16")).
