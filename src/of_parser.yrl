%% -*- erlang -*-

Nonterminals
rule rules condition action keyword name.

Terminals
'(' ')' ',' '.' '[' ']' str kw cmp num kw_skip kw_arrow kw_count kw_send
kw_drop.

%%%===================================================================
%%% Root statement
%%%===================================================================
Rootsymbol rules.

rules -> rule : ['$1'].
rules -> rule rules : ['$1'] ++ '$2'.


name      -> keyword : '$1'.
name      -> kw_drop :  "drop".

rule      -> name '(' condition ')' kw_arrow action '.' : {'$1', '$3', '$6'}.
rule      -> name '(' ')' kw_arrow action '.' : {'$1', undefined, '$5'}.


condition -> keyword : {exists, '$1'}.
condition -> keyword cmp num : {unwrap('$2'), '$1', unwrap('$3')}.


action    -> kw_skip : skip.
action    -> kw_drop : drop.
action    -> kw_send : send.
action    -> kw_count '(' keyword ',' keyword ')' : {count, '$3', '$5'}.

keyword   -> kw : unwrap('$1').

%%%===================================================================
%%% Erlang code.
%%%===================================================================

Erlang code.
-ignore_xref([format_error/1, parse_and_scan/1, return_error/2]).

unwrap({_,_,V}) -> V;
unwrap({_, V}) -> V.
