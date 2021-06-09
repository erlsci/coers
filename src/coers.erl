-module(coers).

-compile({no_auto_import, [error/1]}).

%% API of Coers
-export([value/1,
         error/1,
         has_error/1,
         is_ascii_char/1,
         maybe_string/1,
         to_string/1,
         to_string/2,
         of_string/1,
         of_string/2,
         numeric_align/1,
         to_int/1,
         to_int/2,
         to_float/1,
         to_float/2,
         to_atom/1,
         to_atom/2,
         to_bool/1,
         to_bool/2,
         to_rational/1,
         to_rational/2
        ]).

-include_lib("results/include/results.hrl").

-define(RATIO_REGEX, "^(?<SIGN>[+-])?(?<NUM>\\d+)/(?<DEN>\\d+)$").
-define(NUM_REGEX, "^[+-]?(\\d+([.]\\d*)?([eE][+-]?\\d+)?|[.]\\d+([eE][+-]?\\d+)?)$").

-spec value(result()) -> term().
value(Result) ->
    results:value(Result).

-spec error(result()) -> term().
error(Result) ->
    results:error(Result).

-spec has_error(result()) -> boolean().
has_error(Result) ->
    results:has_error(Result).

%% @doc determine if an integer is a potential Ascii Char
-spec is_ascii_char(integer()) -> boolean().
is_ascii_char(X) when is_integer(X) ->
    (X >= 32) and (X < 127);
is_ascii_char([H]) ->
    is_ascii_char(H);
is_ascii_char(_) ->
    false.

%% @doc check if a list is maybe a string
-spec maybe_string(list()) -> boolean().
maybe_string(List) when is_list(List) ->
    lists:all(fun is_ascii_char/1, List);
maybe_string(_) -> false.

%% @doc try to coerce term into string
-spec to_string((binary() | [any()])) -> result().
to_string(Term) when is_bitstring(Term) ->
    List = binary_to_list(Term),
    to_string(List);
to_string(Term) ->
    case maybe_string(Term) of
        true -> results:new(Term);
        false ->
            List = io_lib:format("~p", [Term]),
            results:new(lists:flatten(List))
    end.

%% @doc Replace value if coercion failed
%%      the suceeded flag is preserved
-spec to_string(term(), term()) -> result().
to_string(Term, Default) when is_record(Default, result) ->
    results:attempt(to_string(Term), Default);
to_string(Term, Default) ->
    results:attempt(to_string(Term), to_string(Default)).

%% @doc an ugly and magic coercion from string to term()
-spec of_string(string()) -> result().
of_string(String) ->
    {ok, Regexp} = re:compile("^.+(\\,|\\;|\\.)$"),
    S =
        case re:run(String, Regexp) of
            {match, [_, {Offset, _}]} ->
                Substring = string:substr(String, 1, Offset -1),
                Substring ++ ".";
            _ -> String ++ "."
        end,
    case erl_scan:string(S) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Exprs} ->
                    {value, Val, []} = erl_eval:exprs(Exprs, []),
                    results:new(Val);
                {error, {_, erl_parse, _}} ->
                    %% TODO extract the error message and add to new_error
                    format_error_msg(erl_parse, "Could not convert string ~p", [String]);
                {error, {Err, A, B}} ->
                    %% TODO extract the error message and add to new_error
                    format_error_msg(Err, "Could not convert string ~p, ~p, ~p", [String, A, B])
            end;
        {error, {Err, A, B}, _} ->
            %% TODO extract the error message and add to new_error
            format_error_msg(Err, "Could not convert string ~p, ~p, ~p", [String, A, B])
    end.

%% @doc try coercion or define a default value the suceeded flag is preserved
-spec of_string(string(), term()) -> result().
of_string(Term, Default) when is_record(Default, result) ->
    results:attempt(of_string(Term), Default);
of_string(Term, Default) ->
    results:attempt(of_string(Term), of_string(Default)).

%% @doc numeric alignement of a string (float or int)
-spec numeric_align(string()) -> atom().
numeric_align(String) ->
    {ok, RatioRegex} = re:compile(?RATIO_REGEX),
    case re:run(String, RatioRegex) of
        {match, _} -> rational;
        _ -> numeric_alignt_int_float(String)
    end.

-spec numeric_alignt_int_float(string()) -> atom().
numeric_alignt_int_float(String) ->
    {ok, Regexp} = re:compile(?NUM_REGEX),
    case re:run(String, Regexp) of
        {match, [_, _]} -> integer;
        {match, [_, _, _]} -> float;
        {match, [_, _, _, _]} -> float;
        _ -> any
    end.

%% @doc try to coerce a term to an integer
-spec to_int(term()) -> result().
to_int(Obj) when is_integer(Obj) -> results:new(Obj);
to_int(Obj) when is_bitstring(Obj) -> to_int(binary_to_list(Obj));
to_int(Obj) when is_list(Obj)    ->
    try list_to_integer(Obj) of
        Val    -> results:new(Val)
    catch error:Err ->
            case numeric_align(Obj) of
                float -> to_int(list_to_float(Obj));
                Type     -> format_error_msg(Err, "Could not convert ~p (type ~p) to int", [Obj, Type])
            end
    end;
to_int(Obj) when is_atom(Obj)     ->
    try Soft = atom_to_list(Obj), to_int(Soft) of
        Result     -> Result
    catch  error:Err ->
            format_error_msg(Err, "Could not convert ~p to int", [Obj])
    end;
to_int(Obj) -> format_error_msg(error, "Could not convert ~p to int", [Obj]).

%% @doc try coercion or define a default value
%%      the suceeded flag is preserved
-spec to_int(term(), term()) -> result().
to_int(Term, Default) when is_record(Default, result) ->
    results:attempt(to_int(Term), Default);
to_int(Term, Default) ->
    results:attempt(to_int(Term), to_int(Default)).


%% @doc try to coerce a term to a float
-spec to_float(term()) -> result().
to_float(Obj) when is_float(Obj)     -> results:new(Obj);
to_float(Obj) when is_bitstring(Obj) -> to_float(binary_to_list(Obj));
to_float(Obj) when is_list(Obj)      ->
    try list_to_float(Obj) of
        Val     -> results:new(Val)
    catch error:Err ->
            case numeric_align(Obj) of
                integer -> to_float(list_to_integer(Obj));
                Type     -> format_error_msg(Err, "Could not convert ~p (type ~p) to float", [Obj, Type])
            end
    end;
to_float(Obj) when is_atom(Obj) ->
    try Obj2 = atom_to_list(Obj), to_float(Obj2) of
        Result -> Result
    catch error:Err ->
            format_error_msg(Err, "Could not convert ~p to flost", [Obj])
    end;
to_float(Obj) ->
    format_error_msg(error, "Could not convert ~p to flost", [Obj]).

%% @doc try coercion or define a default value the suceeded flag is preserved
-spec to_float(term(), term()) -> result().
to_float(Term, Default) when is_record(Default, result) ->
    results:attempt(to_float(Term), Default);
to_float(Term, Default) ->
    results:attempt(to_float(Term), to_float(Default)).

%% @doc try to coerce a term to an atom
-spec to_atom(term()) -> result().
to_atom(Obj) when is_atom(Obj)  -> results:new(Obj);
to_atom(Obj) when is_list(Obj)  ->
    try list_to_atom(Obj) of
        Val     -> results:new(Val)
    catch Err:_ -> results:new_error(Err)
    end;
to_atom(Obj) ->
    Pred = to_string(Obj),
    to_atom(results:value(Pred)).

%% @doc try coercion or define a default value the suceeded flag is preserved
-spec to_atom(term(), term()) -> result().
to_atom(Term, Default) when is_record(Default, result) ->
    results:attempt(to_atom(Term), Default);
to_atom(Term, Default) ->
    results:attempt(to_atom(Term), to_atom(Default)).

%% @doc try to coerce a term to a boolean
-spec to_bool(term()) -> result().
to_bool(Obj) when is_atom(Obj) ->
    results:new(not (Obj == false));
to_bool(Obj) when is_list(Obj) ->
    case string:to_lower(Obj) of
        "true"   -> results:new(true);
        "false"  -> results:new(false);
        _        -> results:new(true)
    end;
to_bool(X) when is_bitstring(X) ->
    Pred = to_string(X),
    to_bool(results:value(Pred));
to_bool(0)   -> results:new(false);
to_bool(0.0) -> results:new(false);
to_bool(1)   -> results:new(true);
to_bool(1.0) -> results:new(true);
to_bool(_)   -> results:new(true).

%% @doc try coercion or define a default value the suceeded flag is preserved
-spec to_bool(term(), term()) -> result().
to_bool(Term, Default) ->
    results:attempt(to_bool(Term), Default).

-spec to_rational(term()) -> result().
to_rational(Obj) when is_list(Obj) ->
    case re:run(Obj, ?RATIO_REGEX, [{capture, ['SIGN', 'NUM', 'DEN'], list}]) of
        {match, [Sign, Num, Denom]} -> 
            results:new(rationals:new(results:value(to_int(Sign ++ Num)), results:value(to_int(Denom))));
        Err ->
            format_error_msg(Err, "Could not convert ~p to ratio", [Obj])
    end;
to_rational(Obj) when is_bitstring(Obj) ->
    to_rational(binary_to_list(Obj));
to_rational({Num, Denom}=Obj) when is_tuple(Obj) ->
    results:new(rationals:new(results:value(to_int(Num)), results:value(to_int(Denom)))).

-spec to_rational(term(), term()) -> result().
to_rational(Term, Default) when is_record(Default, result) ->
    results:attempt(to_rational(Term), Default);
to_rational(Term, Default) ->
    results:attempt(to_rational(Term), to_rational(Default)).

%% Private functions

format_error_msg(Err, Msg, FmtArgs) ->
    results:new_error({Err, lists:flatten(io_lib:format(Msg, FmtArgs))}).
