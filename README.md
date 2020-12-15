# coers

[![Project Logo][logo]][logo-large]

*A small library for coercion to primitive Erlang types*

## About

Coers is a very small library to provide small coercion
on primitive types in Erlang. This library was built
essentially for internal tools at derniercri.io

## Build & Test

    $ # Compile the library
    $ rebar3 compile
    $ # run the tests using eUnit
    $ rebar3 eunit

## Usage

Each coercion is wrapped into a special record: 

```erlang
-record(result, {
  succeeded :: boolean(),
  value     :: term()
}).
```

If a coercion fail, the `value` member is assigned with a default value and the `succeed`
member is `false`. If the coersion succeed, the `value` member becomes the coerced data and the 
`succeed` member becomes `true`.

You can use these 3 combinators to have information about coercion status : 

-  `-spec succeed(result()) -> boolean().`
-  `-spec fail(result()) -> boolean().`
-  `-spec value(result()) -> term().`

For example : 

```shell
1> X = coers:to_int("10").
{result,true,10}
2> Y = coers:to_int("foo").
{result,false,0}
3> [coers:succeed(X), coers:succeed(Y), coers:fail(X), coers:fail(Y)].
[true,false,false,true]
4> [coers:value(X), coers:value(Y)].
[10,0]
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

Copyright © 2016, Xavier van De Woestyne

Copyright © 2020, Duncan McGreggor <oubiwann@gmail.com>.


[//]: ---Named-Links---

[logo]: priv/images/logo-v1.png
[logo-large]: priv/images/logo-v1.svg
