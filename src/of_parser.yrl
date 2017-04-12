%% -*- erlang -*-

Nonterminals
rule rules condition action keyword name count_path count_element.

Terminals
'(' ')' ',' '.' '[' ']' str kw cmp num kw_skip kw_arrow kw_count kw_send
kw_drop kw_at kw_cont.

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
