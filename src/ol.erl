-module(ol).
-include("otters.hrl").
-export([span/1, compile/1, clear/0, check/2]).

compile(S) ->
    {ok, T, _} = of_lexer:string(S),
    {ok, Rs} = of_parser:parse(T),
    {ok, Cs} = group_rules(Rs),
    Rendered = render(Cs),
    io:format("~s~n", [Rendered]),
    application:set_env(otters, filter_string, S),
    dynamic_compile:load_from_string(lists:flatten(Rendered)).

clear() ->
    application:set_env(otters, filter_string, undefined),
    code:purge(ol_filter),
    code:delete(ol_filter).

check(Tags, Span) ->
    {ok, Actions} =  ol_filter:check(Tags),
    perform(Actions, Span).

%% Since dialyzer will arn that the 'dummy'/empty implementation
%% of ol_filter can't ever match send or cout we have to ignore
%% this function
-dialyzer({nowarn_function, perform/2}).
perform([], _Span) ->
    ok;
perform([send | Rest], Span) ->
    otters_conn_zipkin:store_span(Span),
    perform(Rest, Span);
perform([{count, Path} | Rest], Span) ->
    otters_snapshot_count:snapshot(Path, Span),
    perform(Rest, Span).

span(#span{tags = Tags, name = Name, duration = Duration} = Span) ->
    Tags1 = Tags#{
              <<"otters_span_name">>     => {Name, undefined},
              <<"otters_span_duration">> => {Duration, undefined}
             },
    check(Tags1, Span).

%%%===================================================================
%%% Internal functions
%%%===================================================================


group_rules([{Name, Test, Result} | Rest]) ->
    group_rules(Rest, Name, [{Test, Result}], []).

group_rules([{Name, Test, Result} | Rest], Name, Conditions, Acc) ->
    group_rules(Rest, Name, [{Test, Result} | Conditions], Acc);
group_rules([{Name, Test, Result} | Rest], LastName, Conditions, Acc) ->
    case lists:keyfind(Name, 1, Acc) of
        false ->
            Acc1 = [{LastName, lists:reverse(Conditions)} | Acc],
            group_rules(Rest, Name, [{Test, Result}], Acc1);
        _ ->
            {error, {already_defined, Name}}
    end;
group_rules([], LastName, Conditions, Acc) ->
    Acc1 = [{LastName, lists:reverse(Conditions)} | Acc],
    {ok, lists:reverse(Acc1)}.

render(Cs) ->
    ["-module(ol_filter).\n",
     "-export([check/1]).\n",
     "-compile(inline).\n",
     "\n",
     "check(Tags) ->\n",
     "  ", rule_name(0), "(Tags, []).\n",
     "\n",
     "get_tag(Key, Tags) ->\n",
     "  KeyBin = otters_lib:to_bin(Key),\n",
     "  case maps:find(KeyBin, Tags) of\n",
     "    {ok, {V, _}} -> V;\n",
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

render_clauses(Name, [{{exists, Key}, Action} | R], NextRule, N) ->
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := _}, Acc) ->\n", [Key]),
     "  ", render_action(Action, Name, N, NextRule), ";\n",
     rule_name(Name, N), "(Tags, Acc) ->\n",
     "  ", rule_name(Name, N+1), "(Tags, Acc).\n",
     render_clauses(Name, R, NextRule, N + 1)];


%% If we have mutliple continues in a row we can combine them,
%% this combines two continues.

render_clauses(Name, [{{Cmp1, Key1, V1}, continue},
                      {{Cmp2, Key2, V2}, continue}| R], NextRule, N) ->
    {Body, R1} = continue_body(R, Name, N, NextRule),
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := {_V1, _},"
                   " <<\"~s\">> := {_V2, _}}, Acc) "
                   "when _V1 ~s ~s, "
                   " _V2 ~s ~s ->\n",
                   [Key1, Key2, Cmp1, format_v(V1), Cmp2, format_v(V2)]),
     "  ", Body, ";\n",
     rule_name(Name, N), "(Tags, Acc) ->\n",
     "   ", rule_name(NextRule), "(Tags, Acc).\n",
     render_clauses(Name, R1, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, Key, V}, continue} | R], NextRule, N) ->
    {Body, R1} = continue_body(R, Name, N, NextRule),
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := {_V, _}}, Acc) when _V ~s ~s ->\n",
                   [Key, Cmp, format_v(V)]),
     "  ", Body, ";\n",
     rule_name(Name, N), "(Tags, Acc) ->\n",
     "   ", rule_name(NextRule), "(Tags, Acc).\n",
     render_clauses(Name, R1, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, Key, V}, Action} | R], NextRule, N) ->
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := {V, _}}, Acc) when V ~s ~s ->\n",
                   [Key, Cmp, format_v(V)]),
     "  ", render_action(Action, Name, N, NextRule), ";\n",
     rule_name(Name, N), "(Tags, Acc) ->\n",
     "  ", rule_name(Name, N+1), "(Tags, Acc).\n",
     render_clauses(Name, R, NextRule, N + 1)];

render_clauses(Name, [{undefined, Action} | R], NextRule, N) ->
    [rule_name(Name, N), "(Tags, Acc) ->\n"
     "  ", render_action(Action, Name, N, NextRule), ".\n",
     render_clauses(Name, R, NextRule, N + 1)].

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


%% If a continue is followed by a always matching clause
%% We also pull the clause in
continue_body([{undefined, Action}| R], Name, N, NextRule) ->
    {render_action(Action, Name, N, NextRule), R};
continue_body(R, Name, N, _NextRule) ->
    {rule_name(Name, N+1), "(Tags, Acc)", R}.
