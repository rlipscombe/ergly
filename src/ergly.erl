-module(ergly).
-export([main/1]).

-define(red(S), "\e[0;91m" S "\e[0m").
-define(green(S), "\e[0;92m" S "\e[0m").
-define(yellow(S), "\e[0;93m" S "\e[0m").

main([]) ->
    io:format("./ergly foo.erl\n");
main([Filename]) ->
    {ok, Forms} = epp_dodger:parse_file(Filename),
    Comments = erl_comment_scan:file(Filename),
    Module = erl_recomment:recomment_forms(Forms, Comments),
    Tree = module(Module),
    write_module(Tree).

%% If we're passed a list of forms, group them into a single tree and retry.
module(Forms) when is_list(Forms) ->
    module(erl_syntax:form_list(Forms));
module(Forms) ->
    Forms1 = erl_syntax:flatten_form_list(Forms),
    io:format("~p\n", [Forms1]).

write_module(X) ->
    ok.

%% @doc form_list is the root of a re-commented parse tree.
form({tree, form_list, _Location, Forms}) ->
    form_list(Forms);
%% @doc attribute is, e.g., "-module".
form({tree, attribute, _, Attribute}) ->
    attribute(Attribute);
form(Form) ->
    unknown(Form).

form_list([]) ->
    ok;
form_list([Form | Rest]) ->
    form(Form),
    form_list(Rest).

attribute({attribute, Atom, Args}) ->
    io:format("-~s(~p)\n", [Atom, Args]).

unknown(X) ->
    io:format(?red("~p\n"), [X]).
