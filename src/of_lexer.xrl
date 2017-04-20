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

Definitions.
NUM       = [0-9]+
WS        = ([\000-\s]|%.*)
Str1      = '([^']|\.)+'
%'% damn you syntax highlighter
Str2      = "([^"]|\.)+"
%"% damn you syntax highlighter
Instance  = instance
KW        = [A-Za-z][A-Za-z0-9_@-]*
Comp      = (>|>=|<|=<|==|/=)
Arrow     = [-][>]
Skip      = skip
Drop      = drop
Send      = send
Cont      = continue
Count     = count
At        = [@]


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
{Str1}      : S = strip(TokenChars,   TokenLen),
              {token, {str,           TokenLine, S}}.
{Str2}      : S = strip(TokenChars,   TokenLen),
              {token, {str,           TokenLine, S}}.

[(),.]      : {token, {a(TokenChars), TokenLine}}.
{WS}+       : skip_token.


Erlang code.

-ignore_xref([format_error/1, string/2, token/2, token/3, tokens/2, tokens/3]).

strip(TokenChars, TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).
%%unerl(TokenChars, TokenLen) -> lists:sublist(TokenChars, 6, TokenLen).

a(L) -> list_to_atom(L).
i(L) -> list_to_integer(L).

-dialyzer({nowarn_function, yyrev/2}).
