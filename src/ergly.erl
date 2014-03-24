-module(ergly).
-export([main/1]).

-define(red(S), "\e[0;91m" S "\e[0m").
-define(green(S), "\e[0;92m" S "\e[0m").
-define(yellow(S), "\e[0;93m" S "\e[0m").

%-define(VERBOSE, 1).

-ifdef(VERBOSE).
-define(verbose(X), io:format(standard_error, ?yellow("~p\n"), [Node])).
-else.
-define(verbose(X), ok).
-endif.

%% As we traverse the syntax tree, we'll re-enter format, but we need to
%% remember stuff from higher up, such as the current function name, etc.
%% That's kept in the context.
-record(ctxt, {
        indent, % a string containing a bunch of spaces.
        clause  % for formatting clauses, what function (e.g.) are we in?
        }).

%% @todo Do contexts need nesting? If so, do we nest contexts, or do we nest
%% parts of the context?  Thinking nest contexts, where C1 is derived from C0,
%% so the topmost context represents the complete context.

main([]) ->
    io:format("./ergly foo.erl\n");
main([Filename]) ->
    file(Filename).

file(Filename) ->
    {ok, Forms} = epp_dodger:parse_file(Filename),
    Comments = erl_comment_scan:file(Filename),
    Module = erl_recomment:recomment_forms(Forms, Comments),
    module(Module).

module(Module) ->
    io:format(lists:flatten(format(Module))).

format(Node) ->
    format(Node,
           #ctxt{
            indent = ""
            }).

format(Node, Ctxt) ->
    format(erl_syntax:type(Node), Node, Ctxt).

format(atom, Node, _Ctxt) ->
    erl_syntax:atom_literal(Node);

format(integer, Node, _Ctxt) ->
    erl_syntax:integer_literal(Node);

format(string, Node, _Ctxt) ->
    erl_syntax:string_literal(Node);

format(variable, Node, _Ctxt) ->
    erl_syntax:variable_literal(Node);

format(list, Node, Ctxt) ->
    Node1 = erl_syntax:compact_list(Node),
    ["[",
     list(erl_syntax:list_prefix(Node1), Ctxt),
     case erl_syntax:list_suffix(Node1) of
            none -> "";
            Suffix -> 
                seq(Suffix, Ctxt)
        end,
     "]"];

format(arity_qualifier, Node, Ctxt) ->
    F = erl_syntax:arity_qualifier_body(Node),
    A = erl_syntax:arity_qualifier_argument(Node),
    [format(F, Ctxt), "/", format(A, Ctxt)];

format(function, Node, Ctxt) ->
    Name = format(erl_syntax:function_name(Node)),
    [nl(Ctxt), format_clauses(Name, erl_syntax:function_clauses(Node), indent(Ctxt)),
     "."];

format(clause, Node, Ctxt = #ctxt{clause = {function, Name}}) ->
    _P = erl_syntax:clause_patterns(Node),
    _G = erl_syntax:clause_guard(Node),
    B = erl_syntax:clause_body(Node),
    [Name, "() ->", nl(Ctxt), seq(B, Ctxt)];

format(application, Node, Ctxt) ->
    Op = erl_syntax:application_operator(Node),
    Args = erl_syntax:application_arguments(Node),
    [format(Op, Ctxt), "(", seq(Args, Ctxt), ")"];

format(module_qualifier, Node, Ctxt) ->
    Arg = erl_syntax:module_qualifier_argument(Node),
    Body = erl_syntax:module_qualifier_body(Node),
    [format(Arg, Ctxt), ":", format(Body, Ctxt)];

format(form_list, Node, Ctxt) ->
    [seq(erl_syntax:form_list_elements(Node), Ctxt), nl(Ctxt)];

format(attribute, Node, Ctxt) ->
    ?verbose(Node),
    Name = erl_syntax:attribute_name(Node),
    Args = erl_syntax:attribute_arguments(Node),
    ["-", format(Name), "(", seq(Args, Ctxt), ").", nl(Ctxt)];

format(Type, Node, _Ctxt) ->
    io_lib:format(?red("~p: ~p\n"), [Type, Node]).

format_clauses(Name, Clauses, Ctxt) ->
    seq(Clauses, Ctxt#ctxt{clause = {function, Name}}).

seq(Xs, Ctxt) ->
    lists:map(fun(X) -> format(X, Ctxt) end, Xs).

list([], _Ctxt) ->
    [];
list([X], Ctxt) ->
    format(X, Ctxt);
list([X|Xs], Ctxt) ->
    [format(X, Ctxt), ", ", list(Xs, Ctxt)].

indent(Ctxt = #ctxt{indent = Indent}) ->
    Ctxt#ctxt{indent = "    " ++ Indent}.

%% @todo This isn't a good way to output indents. It means that we have to trim
%% the output later.
nl(_Ctxt = #ctxt{indent = Indent}) ->
    ["\n", Indent].

raw(X) ->
    io_lib:format(?red("~p\n"), [X]).
