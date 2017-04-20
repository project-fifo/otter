%%%-------------------------------------------------------------------
%%% Copyright (c) 2017 Heinz N. Gies
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to
%%% deal in the Software without restriction, including without limitation the
%%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%% sell copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.</br>
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING,
%%% FROM OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%% IN THE SOFTWARE.
%%%
%%% @author Heinz N. Gies <heinz@project-fifo.net>
%%% @copyright (C) 2017, Heinz N. Gies
%%% @doc The ol module is the interface to the otters filter language
%%% it deals with compiling and running compiled filters.
%%%
%%% The otters filter language is a high peroformance language build to
%%% filter open traccing spans. It achives this by compiling directoy to
%%% erlang code that can be run ontop of the erlang virutal machine instead
%%% taking advantage of pattern matching, guards and other features that
%%% are already implemented within the erlang vm.
%%%
%%% The language itself is loosely baed on erlang syntax, and works somewaht
%%% like firewall rules. It comes with two main abstractions, the <em>rule</em>
%%% and the <em>condition</em>, while rules are sets of one or more conditions.
%%% During rule execution all tags can be accessed and compared against, as well
%%% as the two special values <em>otters_span_duration</em> and
%%% <em>otters_span_name</em> reflecting respectively the duration and the
%%% name of the span.
%%%
%%% Rules are executed in order they are written, within the rules conditions
%%% are also excuted within the order they're provided.
%%%
%%% Each rule can have a condition attached, which compares a tag with a value
%%% values can be either strings or numbers. If the condition just is a field
%%% the existence will be tested, if the condition is empty the condition is
%%% always executed once it is reached.
%%%
%%% Each condition can have an action attached. Each action is only excuted
%%% once, so if two rules end with a <b>send</b> action or a <b>count</b>
%%% action for the same counter only one of them is executed. However
%%% <b>count</b> for different counters are all excutred. Possible actions are:
%%% <dl>
%%%   <dt><b>drop</b></dt>
%%%   <dd>Drops this spawn, does not excute futher conditions in this rule
%%%       neither does it excute furhter rules.</dd>
%%%   <dt><b>skip</b></dt>
%%%   <dd>Ships the rest of the rule but continues with the next rule.</dd>
%%%   <dt><b>continue</b></dt>
%%%   <dd>If the condition matches it continues the current rule, otherwise
%%%       skips therest of the rule.</dd>
%%%   <dt><b>send</b></dt>
%%%   <dd>Marks the span to be send.</dd>
%%%   <dt><b>count(string | tag ...)</b></dt>
%%%   <dd>Coun the current span with a given counter name, elements can either
%%%       be a literal string, or a name of a field that will be looked up.</dd>
%%% </dl>
%%%
%%% An example ruleset would be:
%%%
%%% <pre>
%%% %% If our span takes less then 5s skip the rest of the rules
%%% slow_spans(otters_span_duration > 5000000) ->
%%%   continue.
%%% %% Skip requests that are not radius requests
%%% slow_spans(otters_span_name == 'radius request') ->
%%%   continue.
%%% %% Count
%%% slow_spans() ->
%%%   count('long_radius_request').
%%% %% Send
%%% slow_spans() ->
%%%   send.
%%% %% Count them all
%%% count() ->
%%%   count('request', otters_span_name, final_result).
%%% </pre>
%%% @end
%%% Created : 14 Apr 2017 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(ol).
-include_lib("otters/include/otters.hrl").
-export([span/1, load/1, compile/1, clear/0]).

-define(DURATION, "otters_span_duration").
-define(NAME, "otters_span_name").

-ifdef(TEST).
-compile(export_all).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Loads a filter rule file and compiles it
%% @end
%%--------------------------------------------------------------------
load(F) ->
    {ok, B} = file:read_file(F),
    S = binary_to_list(B),
    compile(S).

%%--------------------------------------------------------------------
%% @doc
%% Compiles a filter script and generates the related module.
%% @end
%%--------------------------------------------------------------------
compile(S) ->
    {ok, T, _} = of_lexer:string(S),
    {ok, Rs} = of_parser:parse(T),
    {ok, Cs} = group_rules(Rs),
    Rendered = render(Cs),
    %%io:format("~s~n", [Rendered]),
    application:set_env(otters, filter_string, S),
    case dynamic_compile:load_from_string(lists:flatten(Rendered)) of
        {module, ol_filter} ->
            ok;
        E ->
            E
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes the filter script and module.
%% @end
%%--------------------------------------------------------------------
clear() ->
    compile("drop() -> drop.").

%%--------------------------------------------------------------------
%% @doc
%% Tests a span and performs the requested actions on it.
%% @end
%%--------------------------------------------------------------------
span(Span) ->
    {ok, Actions} = run(Span),
    perform(lists:usort(Actions), Span).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Tests a span and performs the requested actions on it.
%% @end
%%--------------------------------------------------------------------
run(#span{tags = Tags, name = Name, duration = Duration}) ->
    ol_filter:check(Tags, Name, Duration).


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

%%%===================================================================
%%% Compiler functions
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

render([{Name,_} | _] = Cs) ->
    ["-module(ol_filter).\n",
     "-export([check/3]).\n",
     "-compile(inline).\n",
     "\n",
     "check(Tags, Name, Duration) ->\n",
     "  ", rule_name(Name, 0), "(Tags, Name, Duration, []).\n",
     "\n",
     "get_tag(Key, Tags) ->\n",
     "  KeyBin = otters_lib:to_bin(Key),\n",
     "  case maps:find(KeyBin, Tags) of\n",
     "    {ok, {V, _}} -> V;\n",
     "    _ -> <<>>\n",
     "  end.\n",
     "\n",
     render_(Cs)].

render_([{Name, Clauses} | [{NextName, _} | _] = R]) ->
    [render_clauses(Name, Clauses, NextName, 0), "\n",
     render_(R)];
render_([{Name, Clauses}]) ->
    [render_clauses(Name, Clauses, undefined, 0), "\n",
     "finish(_Tags, _Name, _Duration, Acc) ->\n",
     "  {ok, Acc}.\n"].

rule_name(undefined, _N) ->
    "finish";
rule_name(Name, N) ->
    ["rule_", Name, "_", integer_to_list(N)].

render_clauses(_Name, [], _NextRule, _N) ->
    "";

render_clauses(Name, [{undefined, drop} | R], NextRule, N) ->
    [rule_name(Name, N), "(_Tags, _Name, _Duration, Acc) ->\n",
     "  {ok, Acc}.\n\n",
     render_clauses(Name, R, NextRule, N + 1)];

%% Special case if we match for duration and name at once
render_clauses(Name, [{{_Cmp, ?DURATION, _V}, continue} = A,
                      {{_Cmp1, ?NAME, _V1}, continue} = B| R],
               NextRule, N) ->
    render_clauses(Name, [B, A| R], NextRule, N);

render_clauses(Name, [{{Cmp, ?NAME, V}, continue},
                      {{Cmp1, ?DURATION, V1}, continue}| R],
               NextRule, N) ->
    {Body, R1} = continue_body(R, Name, N, NextRule),
    [rule_name(Name, N),
     io_lib:format("(Tags, Name, Duration, Acc) when Name ~s ~s,"
                   " Duration ~s ~s->\n",
                   [Cmp, format_v(V), Cmp1, format_v(V1)]),
     "  ", Body, ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "   ", rule_name(NextRule, 0), "(Tags, Name, Duration, Acc).\n\n",
     render_clauses(Name, R1, NextRule, N + 1)];


%% Speical cases for Duration
render_clauses(Name, [{{exists, ?DURATION}, Action} | R], NextRule, N) ->
    [rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "  ", render_action(Action, Name, N, NextRule, R), ".\n\n",
     render_clauses(Name, R, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, ?DURATION, V}, continue} | R],
               NextRule, N) ->
    {Body, R1} = continue_body(R, Name, N, NextRule),
    [rule_name(Name, N),
     io_lib:format("(Tags, Name, Duration, Acc) when Duration ~s ~s ->\n",
                   [Cmp, format_v(V)]),
     "  ", Body, ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "   ", rule_name(NextRule, 0), "(Tags, Name, Duration, Acc).\n\n",
     render_clauses(Name, R1, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, ?DURATION, V}, Action} | R], NextRule, N) ->
    [rule_name(Name, N),
     io_lib:format("(Tags, Name, Duration, Acc) "
                   "when Duration ~s ~s ->\n",
                   [Cmp, format_v(V)]),
     "  ", render_action(Action, Name, N, NextRule, R), ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "  ", next_rule(Name, N, NextRule, R),
     render_clauses(Name, R, NextRule, N + 1)];


%% Speical cases for Name
render_clauses(Name, [{{exists, ?NAME}, Action} | R], NextRule, N) ->
    [rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "  ", render_action(Action, Name, N, NextRule, R), ".\n\n",
     render_clauses(Name, R, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, ?NAME, V}, continue} | R],
               NextRule, N) ->
    {Body, R1} = continue_body(R, Name, N, NextRule),
    [rule_name(Name, N),
     io_lib:format("(Tags, Name, Duration, Acc) when Name ~s ~s ->\n",
                   [Cmp, format_v(V)]),
     "  ", Body, ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "   ", rule_name(NextRule, 0), "(Tags, Name, Duration, Acc).\n\n",
     render_clauses(Name, R1, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, ?NAME, V}, Action} | R], NextRule, N) ->
    [rule_name(Name, N),
     io_lib:format("(Tags, Name, Duration, Acc) "
                   "when Name ~s ~s ->\n",
                   [Cmp, format_v(V)]),
     "  ", render_action(Action, Name, N, NextRule, R), ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "  ", next_rule(Name, N, NextRule, R),
     render_clauses(Name, R, NextRule, N + 1)];

%% Normal cases
render_clauses(Name, [{{exists, Key}, Action} | R], NextRule, N) ->
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := _}, Name, Duration, Acc) ->\n",
                   [Key]),
     "  ", render_action(Action, Name, N, NextRule, R), ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "  ", next_rule(Name, N, NextRule, R),
     render_clauses(Name, R, NextRule, N + 1)];


%% If we have mutliple continues in a row we can combine them,
%% this combines two continues.

render_clauses(Name, [{{Cmp1, Key1, V1}, continue},
                      {{Cmp2, Key2, V2}, continue}| R], NextRule, N) ->
    {Body, R1} = continue_body(R, Name, N, NextRule),
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := {_V1, _},"
                   " <<\"~s\">> := {_V2, _}}, Name, Duration, Acc) "
                   "when _V1 ~s ~s, "
                   " _V2 ~s ~s ->\n",
                   [Key1, Key2, Cmp1, format_v(V1), Cmp2, format_v(V2)]),
     "  ", Body, ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "   ", next_rule(Name, N, NextRule, R1),
     render_clauses(Name, R1, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, Key, V}, continue} | R], NextRule, N) ->
    {Body, R1} = continue_body(R, Name, N, NextRule),
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := {_V, _}}, Name, Duration, Acc) "
                   "when _V ~s ~s ->\n",
                   [Key, Cmp, format_v(V)]),
     "  ", Body, ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "   ", next_rule(Name, N, NextRule, R1),
     render_clauses(Name, R1, NextRule, N + 1)];

render_clauses(Name, [{{Cmp, Key, V}, Action} | R], NextRule, N) ->
    [rule_name(Name, N),
     io_lib:format("(Tags = #{<<\"~s\">> := {_V, _}}, Name, Duration, Acc) "
                   "when _V ~s ~s ->\n",
                   [Key, Cmp, format_v(V)]),
     "  ", render_action(Action, Name, N, NextRule, R), ";\n",
     rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n",
     "  ", next_rule(Name, N, NextRule, R),
     render_clauses(Name, R, NextRule, N + 1)];

render_clauses(Name, [{undefined, Action} | R], NextRule, N) ->
    [rule_name(Name, N), "(Tags, Name, Duration, Acc) ->\n"
     "  ", render_action(Action, Name, N, NextRule, R), ".\n\n",
     render_clauses(Name, R, NextRule, N + 1)].

render_action(drop, _Name, _N, _NextRule, _R) ->
    "{ok, Acc}";
render_action(skip, _Name, _N, NextRule, _R) ->
    [rule_name(NextRule, 0), "(Tags, Name, Duration, Acc)"];

render_action(send, Name, N, NextRule, R) ->
    [next_rule_name(Name, N, NextRule, R), "(Tags, Name, Duration, [send | Acc])"];

render_action({count, Path}, Name, N, NextRule, R) ->
    [next_rule_name(Name, N, NextRule, R),
     "(Tags, Name, Duration, [{count, [", make_path(Path), "]} | Acc])"].


next_rule(Name, N, NextRule, R) ->
    [next_rule_name(Name, N, NextRule, R), "(Tags, Name, Duration, Acc).\n\n"].

next_rule_name(_Name, _N, NextRule, []) ->
    rule_name(NextRule, 0);
next_rule_name(Name, N, _NextRule, _R) ->
    rule_name(Name, N + 1).

make_path([E]) ->
    make_e(E);
make_path([E | R]) ->
    [make_e(E), ", ", make_path(R)].


make_e({get, ?NAME}) ->
    "Name";
make_e({get, ?DURATION}) ->
    "Duration";
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
    {render_action(Action, Name, N, NextRule, R), R};
continue_body(R, Name, N, _NextRule) ->
    {[rule_name(Name, N + 1), "(Tags, Name, Duration, Acc)"], R}.
