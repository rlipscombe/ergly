-module(hello).
-export([say/0]).

say() ->
    io:format("Hello World.\n").
