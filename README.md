# coers

[![Build Status][gh-actions-badge]][gh-actions]
[![Erlang Versions][erlang-badge]][versions]
[![Tag][github-tag-badge]][github-tag]

[![Project Logo][logo]][logo-large]

*A small library for coercion to primitive Erlang types*

## About

Coers is a very small library to provide small coercion on primitive types in Erlang.
This library was built essentially for internal tools at derniercri.io

## Build & Test

``` shell
$ rebar3 compile
$ rebar3 check
```

The later includes not only unit tests, but also other checks and coverage
assessment.

## Usage

Each coercion is wrapped into a [special record](https://github.com/erlsci/results/blob/main/include/results.hrl): 

```erlang
-record(result, {
  value :: term(),
  error :: term(),
}).
```

If a coercion fail, the `value` field is undefined and the `error` field is populated with
an appropriate error. If the coersion succeed, the `value` field becomes the coerced data and the 
`error` field is undefined.

You can use these functions from the coers API to examine the coercion status:

- `coers:value(Result)`
- `coers:error(Result)`
- `coers:has_error(Result)`

For example : 

```erlang
1> R1 = coers:to_int("10").
{result,10,undefined}
2> R2 = coers:to_int("foo").
{result,undefined,{badarg,"Could not convert \"foo\" (type any) to int"}}
```

Additional convenience functions are available via the [results library](https://github.com/erlsci/results):

``` erlang
3> results:has_values([R1, R2]).
[true,false]
4> results:has_errors([R1, R2]).
[false,true]
5> results:values([R1, R2]).
[10,undefined]
```

Note that fractions are supported (via the [rationals](https://github.com/erlsci/rationals) Erlang library):

``` erlang
6> coers:to_rational("1/42").
{result,{fraction,1,42},undefined}
7> coers:to_rational(<<"1/42">>).
{result,{fraction,1,42},undefined}
8> coers:to_rational({1, 42}).
{result,{fraction,1,42},undefined}
```

Example usgage in LFE:

``` lisp
(defun ->lfe (arg)
  (case arg
   (#"#t" 'true)
   (#"#f" 'false)
   (#"NIL" 'nil)
   (_ (cond ((?= `#(result true ,val) (coers:to_int arg))
             val)
            ((?= `#(result true ,val) (coers:to_float arg))
             val)
            ((?= `#(result true ,val) (coers:to_bool arg))
             val)
            ((?= `#(result true ,val) (coers:to_string arg))
             val)
            ('true arg)))))
```

## License

MIT

Copyright © 2020-2021, Erlang-Aided Enrichment Center

Copyright © 2016, Xavier van De Woestyne


[//]: ---Named-Links---

[logo]: priv/images/logo-v1.png
[logo-large]: priv/images/logo-v1.svg
[gh-actions-badge]: https://github.com/erlsci/coers/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/erlsci/coers/actions
[erlang-badge]: https://img.shields.io/badge/erlang-19%20to%2023-blue.svg
[versions]: https://github.com/erlsci/coers/blob/master/.github/workflows/cicd.yml
[github-tag]: https://github.com/erlsci/coers/tags
[github-tag-badge]: https://img.shields.io/github/tag/erlsci/coers.svg
