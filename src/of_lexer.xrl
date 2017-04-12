%% -*- erlang -*-

Definitions.
ERL       = -erl\s.*
NUM       = [0-9]+
WS        = ([\000-\s]|%.*)
Str2      = "([^"]|\.)+"
%"% damn you syntax highlighter
Instance  = instance
KW        = [A-Za-z][A-Za-z0-9_@-]*
Comp      = (>|>=|<|=<|==)
Arrow     = [-][>]
Skip      = skip
Drop      = drop
Send      = send
Cont      = continue
Count     = count


Rules.
%%{ERL}       : {token, {erlang,        TokenLine, unerl(TokenChars, TokenLine)}}.
{Comment}   : {token, {comment,       TokenLine}}.
{Arrow}     : {token, {kw_arrow,      TokenLine}}.
{Send}      : {token, {kw_send,       TokenLine}}.
{Drop}      : {token, {kw_drop,       TokenLine}}.
{Skip}      : {token, {kw_skip,       TokenLine}}.
{Cont}      : {token, {kw_cont,       TokenLine}}.
{Count}     : {token, {kw_count,      TokenLine}}.
{KW}        : {token, {kw,            TokenLine, TokenChars}}.
{Comp}      : {token, {cmp,           TokenLine, a(TokenChars)}}.
{NUM}       : {token, {num,           TokenLine, i(TokenChars)}}.
{Str2}      : S = strip(TokenChars,   TokenLen),
              {token, {str,           TokenLine, S}}.

[(),.[\]]   : {token, {a(TokenChars), TokenLine}}.
{WS}+       : skip_token.


Erlang code.
-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

strip(TokenChars, TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).
%%unerl(TokenChars, TokenLen) -> lists:sublist(TokenChars, 6, TokenLen).

a(L) -> list_to_atom(L).
i(L) -> list_to_integer(L).

-dialyzer({nowarn_function, yyrev/2}).
