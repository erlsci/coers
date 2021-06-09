%% @author X. Van de Woestyne <xaviervdw@gmail.com>
%% @copyright 2016 X. Van de Woestyne
%% @doc coers provide small function for value coercion.

-module(coers).

%% API of Coers
-export([
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
to_string(Term, Default) ->
  results:attempt(to_string(Term), Default).

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
          {error, {_, _, _}} ->
            %% TODO extract the error message and add to new_error
            results:new_error({error, "tbd"})
        end;
      {error, {_, _, _}, _} ->
        %% TODO extract the error message and add to new_error
        results:new_error({error, "tbd"})
  end.

%% @doc try coercion or define a default value the suceeded flag is preserved
-spec of_string(string(), term()) -> result().
of_string(Str, Default) ->
  results:attempt(of_string(Str), Default).

%% @doc numeric alignement of a string (float of int)
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
  catch _:_ ->
    case numeric_align(Obj) of
      float -> to_int(list_to_float(Obj));
      _     -> results:new(0)
    end
  end;
to_int(Obj) when is_atom(Obj)     ->
  try Soft = atom_to_list(Obj), to_int(Soft) of
  Result     -> Result
  catch  _:_ ->
    results:new(0)
  end;
to_int(_) -> results:new(0).

%% @doc try coercion or define a default value
%%      the suceeded flag is preserved
-spec to_int(term(), term()) -> result().
to_int(Term, Default) ->
  results:attempt(to_int(Term), Default).

%% @doc try to coerce a term to a float
-spec to_float(term()) -> result().
to_float(Obj) when is_float(Obj)     -> results:new(Obj);
to_float(Obj) when is_bitstring(Obj) -> to_float(binary_to_list(Obj));
to_float(Obj) when is_list(Obj)      ->
  try list_to_float(Obj) of
  Val     -> results:new(Val)
  catch  _:_ ->
    case numeric_align(Obj) of
      integer -> to_float(list_to_integer(Obj));
      _       -> results:new(0.0)
    end
  end;
to_float(Obj) when is_atom(Obj)      ->
  try Pred = atom_to_list(Obj), to_float(Pred) of
  Result -> Result
  catch _:_ ->
    results:new(0.0)
  end;
to_float(_) -> results:new(0.0).

%% @doc try coercion or define a default value the suceeded flag is preserved
-spec to_float(term(), term()) -> result().
to_float(Term, Default) ->
  results:attempt(to_float(Term), Default).

%% @doc try to coerce a term to an atom
-spec to_atom(term()) -> result().
to_atom(Obj) when is_atom(Obj)  -> results:new(Obj);
to_atom(Obj) when is_list(Obj)  ->
  try list_to_atom(Obj) of
  Val     -> results:new(Val)
  catch _:_ -> results:new(false)
  end;
to_atom(Obj) ->
  Pred = to_string(Obj),
  to_atom(results:value(Pred)).

%% @doc try coercion or define a default value the suceeded flag is preserved
-spec to_atom(term(), term()) -> result().
to_atom(Term, Default) ->
  results:attempt(to_atom(Term), Default).

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
        _ ->
            results:new(rationals:new(0, 1))
    end;
to_rational(Obj) when is_bitstring(Obj) ->
    to_rational(binary_to_list(Obj));
to_rational({Num, Denom}=Obj) when is_tuple(Obj) ->
    results:new(rationals:new(results:value(to_int(Num)), results:value(to_int(Denom)))).

-spec to_rational(term(), term()) -> result().
to_rational(Obj, Default) ->
    results:attempt(to_rational(Obj), results:value(to_rational(Default))).
