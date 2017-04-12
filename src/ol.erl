-module(ol).
-export([compile/1]).

compile(S) ->
    {ok, T, _} = of_lexer:string(S),
    {ok, Rs} = of_parser:parse(T),
    {ok, Cs} = group_rules(Rs),
    Rendered = render(Cs),
    io:format("~s~n", [Rendered]),
    dynamic_compile:load_from_string(lists:flatten(Rendered)).

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
     "-export([check/2]).\n",
     "check(Span, Tags) ->\n",
     "  ", rule_name(0), "(Span, Tags).\n",
     render(Cs, 0)].

render([], N) ->
    [rule_name(N), "(_Span, _Tags) ->\n"
     "  ok.\n"];
render([{Name, Clauses} | R], N) ->
    ["rule_", integer_to_list(N), "(Span, Tags) -> \n",
     "  ", rule_name(Name, 0), "(Span, Tags).\n",
     render_clauses(Name, Clauses, N + 1, 0), "\n",
     render(R, N + 1)].

rule_name(N) ->
    ["rule_", integer_to_list(N)].

rule_name(Name, N) ->
    ["rule_", Name, "_", integer_to_list(N)].

render_clauses(Name, [], NextRule, N) ->
    [rule_name(Name, N), "(Span, Tags) ->\n"
     "  ", rule_name(NextRule), "(Span, Tags)."];
render_clauses(Name, [{undefined, drop} | R], NextRule, N) ->
    [rule_name(Name, N), "(Span, Tags) ->\n",
     "  ok.\n",
     render_clauses(Name, R, NextRule, N + 1)];

render_clauses(Name, [Check | R], NextRule, N) ->
    [rule_name(Name, N), "(Span, Tags) ->\n",
     make_check(Check, Name, N, NextRule),
     render_clauses(Name, R, NextRule, N + 1)].


make_check({undefined, Action}, Name, N, NextRule)  ->
    ["    ", render_action(Action, Name, N, NextRule), ".\n"];
make_check({{Cmp, Key, V}, Action}, Name, N, NextRule)  ->
    [io_lib:format("  case maps:find(<<\"~s\">>, Tags) of\n", [Key]),
     io_lib:format("    {ok, V} when V ~s ~s ->\n", [Cmp, format_v(V)]),
     "      ", render_action(Action, Name, N, NextRule), ";\n",
     "    _ ->\n",
     "      ", rule_name(Name, N+1), "(Span, Tags)\n",
     "  end.\n"].

render_action(drop, _Name, _N, _NextRule) ->
    "ok";
render_action(skip, _Name, _N, NextRule) ->
    rule_name(NextRule);
render_action(send, Name, N, _NextRule) ->
    ["otters_conn_zipkin:store_span(Span),\n",
     "      ", rule_name(Name, N + 1), "(Span, Tags)"];
render_action({count, Prefix, Value}, Name, N, _NextRule) ->
    [io_lib:format("otters_snapshot_count:snapshot([~s, ~s], Span),\n",
                   [Prefix, Value]),
     "      ", rule_name(Name, N + 1), "(Span, Tags)"].



format_v(V) when is_integer(V) ->
    integer_to_list(V);
format_v(V) ->
    V.

