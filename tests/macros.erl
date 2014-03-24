-module(macros).
-define(X, 42).
-ifdef(TEST).
-define(Y, 123).
-else.
-define(Y, 321).
-endif.

