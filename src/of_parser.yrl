%% -*- erlang -*-
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

Nonterminals
rule rules condition action keyword name count_path count_element.

Terminals
'(' ')' ',' '.' str kw cmp num kw_skip kw_arrow kw_count kw_send
kw_drop kw_cont.

%%%===================================================================
%%% Root statement
%%%===================================================================
Rootsymbol rules.

rules -> rule : ['$1'].
rules -> rule rules : ['$1'] ++ '$2'.

name      -> keyword  : '$1'.
name      -> kw_drop  : "drop".
name      -> kw_send  : "send".
name      -> kw_cont  : "continue".
name      -> kw_count : "count".
name      -> kw_skip  : "skip".

rule      -> name '(' condition ')' kw_arrow action '.' : {'$1', '$3', '$6'}.
rule      -> name '(' ')' kw_arrow action '.' : {'$1', undefined, '$5'}.


condition -> keyword : {exists, '$1'}.
condition -> keyword cmp num : {unwrap('$2'), '$1', unwrap('$3')}.
condition -> keyword cmp str : {unwrap('$2'), '$1', unwrap('$3')}.


action    -> kw_skip : skip.
action    -> kw_drop : drop.
action    -> kw_send : send.
action    -> kw_cont : continue.
action    -> kw_count '(' count_path ')' : {count, '$3'}.

count_element -> keyword : {get, '$1'}.
count_element -> str : unwrap('$1').

count_path -> count_element : ['$1'].
count_path -> count_element ',' count_path : ['$1'] ++ '$3'.


keyword   -> kw : unwrap('$1').

%%%===================================================================
%%% Erlang code.
%%%===================================================================

Erlang code.
-ignore_xref([format_error/1, parse_and_scan/1, return_error/2]).

unwrap({_,_,V}) -> V;
unwrap({_, V}) -> V.
