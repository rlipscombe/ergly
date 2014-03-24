-module(hello).
-export([foo/0, bar/1, baz/2]).

foo() ->
    ok.

bar(X) ->
    X.

baz(X, Y) ->
    X * Y.
