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
%%%-------------------------------------------------------------------

-module(ol_eqc).

-compile({parse_transform,eqc_grammar}).
-eqc_grammar({yecc_tokens,"../src/of_parser.yrl"}).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).


'('() ->
    {'(', nat()}.
')'() ->
    {')', nat()}.
','() ->
    {',', nat()}.
'.'() ->
    {'.', nat()}.


kw_arrow() ->
    {kw_arrow, nat()}.
kw_cont() ->
    {kw_arrow, nat()}.
kw_count() ->
    {kw_count, nat()}.
kw_drop() ->
    {kw_drop, nat()}.
kw_send() ->
    {kw_send, nat()}.
kw_skip() ->
    {kw_skip, nat()}.

cmp() ->
    {cmp, nat(), oneof(['>', '>=', '<', '=<', '==', '/='])}.

kw() ->
    {kw, nat(), kw_()}.

num() ->
    {num, nat(), nat()}.

str() ->
    {str, nat(), kw_()}.

kw_() ->
    ?LET(K, ?SIZED(Size, kw_(Size)), lists:flatten([K])).

kw_(0) ->
    [letter()];
kw_(N) ->
    [kw_(N - 1), oneof([letter(), choose($0, $9), $_])].

letter() ->
    oneof([
           choose($a, $z),
           choose($A, $Z)
          ]).

%% Generating:
%% [{kw_arrow,0},{'(',0},{')',0},{kw_arrow,0},{kw_skip,0},{'.',0}]
no_prop_parse() ->
    ?FORALL(SymbolicExpr, rule(),
            begin
                Tokens = eqc_grammar:eval(SymbolicExpr),

                case of_parser:parse(Tokens) of
                    {ok, _SyntaxTree} ->
                        true;
                    {error, E} ->
                        %%io:format("~p\n~p -> ~p~n", [SymbolicExpr, Tokens, E]),
                        false
                end
            end).

prop_compile() ->
    ?FORALL(SymbolicExpr, rule(),
            begin
                Tokens = eqc_grammar:eval(SymbolicExpr),
                case of_parser:parse(Tokens) of
                    {ok, Rs} ->
                        {ok, Cs} = ol:group_rules(Rs),
                        Rendered = ol:render(Cs),
                        F = lists:flatten(Rendered),
                        Res = try
                                  dynamic_compile:load_from_string(F)
                              catch
                                  _:E ->
                                      E
                              end,
                        ?WHENFAIL(io:format(user, "~s~n=>~p~n", [F, Res]),
                                  Res =:= {module, ol_filter});
                    {error, _E} ->
                        true
                end
            end).

prop_filter() ->
    ?FORALL(SymbolicExpr, rule(),
            ?FORALL(SpanR, thrift_eqc:span(),
                    begin
                        Tokens = eqc_grammar:eval(SymbolicExpr),
                        Span = eval(SpanR),
                        case of_parser:parse(Tokens) of
                            {ok, Rs} ->
                                {ok, Cs} = ol:group_rules(Rs),
                                Rendered = ol:render(Cs),
                                F = lists:flatten(Rendered),
                                dynamic_compile:load_from_string(F),
                                {ok, _} = ol:run(Span),
                                true;
                            {error, _E} ->
                                true
                        end
                    end)).
