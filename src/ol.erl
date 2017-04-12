-module(ol).
-export([compile/1, clear/0]).

compile(S) ->
    {ok, T, _} = of_lexer:string(S),
    {ok, Rs} = of_parser:parse(T),
    {ok, Cs} = group_rules(Rs),
    Rendered = render(Cs),
    io:format("~s~n", [Rendered]),
    dynamic_compile:load_from_string(lists:flatten(Rendered)).

clear() ->
    code:purge(ol_filter),
    code:delete(ol_filter).

group_rules([{Name, Test, Result} | Rest]) ->
    group_rules(Rest, Name, [{Test, Result}], []).

group_rules([{Name, Test, Result} | Rest], Name, Conditions, Acc) ->
    group_rules(Rest, Name, [{Test, Result} | Conditions], Acc);
group_rules([{Name, Test, Result} | Rest], LastName, Conditions, Acc) ->
    case lists:keyfind(Name, 1, Acc) of
        true ->
            {error, {already_defined, Name}};
        false ->
            Acc1 = [{LastName, lists:reverse(Conditions)} | Acc],
            group_rules(Rest, Name, [{Test, Result}], Acc1)
    end;
group_rules([], LastName, Conditions, Acc) ->
    Acc1 = [{LastName, lists:reverse(Conditions)} | Acc],
    {ok, lists:reverse(Acc1)}.

render(Cs) ->
    ["-module(ol_filter).\n",
     "-export([check/1]).\n",
     "\n",
     "check(Tags) ->\n",
     "  ", rule_name(0), "(Tags, []).\n",
     "\n",
     "get_tag(Key, Tags) ->\n",
     "  KeyBin = otters_lib:to_bin(Key),\n",
     "  case maps:find(KeyBin, Tags) of\n",
     "    {ok, V} -> V;\n",
     "    _ -> <<>>\n",
     "  end.\n",
     "\n",
     render(Cs, 0)].

render([], N) ->
    [rule_name(N), "(_Tags, Acc) ->\n"
     "  {ok, Acc}.\n"];
render([{Name, Clauses} | R], N) ->
    ["rule_", integer_to_list(N), "(Tags, Acc) -> \n",
     "  ", rule_name(Name, 0), "(Tags, Acc).\n",
     render_clauses(Name, Clauses, N + 1, 0), "\n",
     render(R, N + 1)].

rule_name(N) ->
    ["rule_", integer_to_list(N)].

rule_name(Name, N) ->
    ["rule_", Name, "_", integer_to_list(N)].

render_clauses(Name, [], NextRule, N) ->
    [rule_name(Name, N), "(Tags, Acc) ->\n"
     "  ", rule_name(NextRule), "(Tags, Acc)."];
render_clauses(Name, [{undefined, drop} | R], NextRule, N) ->
    [rule_name(Name, N), "(_Tags, Acc) ->\n",
     "  Acc.\n",
     render_clauses(Name, R, NextRule, N + 1)];

render_clauses(Name, [Check | R], NextRule, N) ->
    [rule_name(Name, N), "(Tags, Acc) ->\n",
     make_check(Check, Name, N, NextRule),
     render_clauses(Name, R, NextRule, N + 1)].


make_check({undefined, Action}, Name, N, NextRule)  ->
    ["    ", render_action(Action, Name, N, NextRule), ".\n"];
make_check({{exists, Key}, Action}, Name, N, NextRule)  ->
    [io_lib:format("  case maps:find(<<\"~s\">>, Tags) of\n", [Key]),
     "    {ok, _V} ->\n",
     "      ", render_action(Action, Name, N, NextRule), ";\n",
     "    _ ->\n",
     "      ", rule_name(Name, N+1), "(Tags, Acc)\n",
     "  end.\n"];
make_check({{Cmp, Key, V}, continue}, Name, N, NextRule)  ->
    [io_lib:format("  case maps:find(<<\"~s\">>, Tags) of\n", [Key]),
     io_lib:format("    {ok, V} when V ~s ~s ->\n", [Cmp, format_v(V)]),
     "      ", rule_name(Name, N+1), "(Tags, Acc);\n",
     "    _ ->\n",
     "      ", rule_name(NextRule), "(Tags, Acc)\n",
     "  end.\n"];
make_check({{Cmp, Key, V}, Action}, Name, N, NextRule)  ->
    [io_lib:format("  case maps:find(<<\"~s\">>, Tags) of\n", [Key]),
     io_lib:format("    {ok, V} when V ~s ~s ->\n", [Cmp, format_v(V)]),
     "      ", render_action(Action, Name, N, NextRule), ";\n",
     "    _ ->\n",
     "      ", rule_name(Name, N+1), "(Tags, Acc)\n",
     "  end.\n"].

render_action(drop, _Name, _N, _NextRule) ->
    "{ok, Acc}";
render_action(skip, _Name, _N, NextRule) ->
    [rule_name(NextRule), "(Tags, Acc)"];
render_action(send, Name, N, _NextRule) ->
    [rule_name(Name, N + 1), "(Tags, [send | Acc])"];
render_action({count, Path}, Name, N, _NextRule) ->
    [rule_name(Name, N + 1),
     "(Tags, [{count, [", make_path(Path), "]} | Acc])"].


make_path([E]) ->
    make_e(E);
make_path([E | R]) ->
    [make_e(E), ", ", make_path(R)].


make_e({get, V}) ->
    ["get_tag(", format_v(V), ", Tags)"];
make_e(V) ->
    format_v(V).

format_v(V) when is_integer(V) ->
    integer_to_list(V);
format_v(V) ->
    ["<<\"", V, "\">>"].

