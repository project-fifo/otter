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
%%% @doc High perofrmance encoder for the zapkin thrift protocol.
%%% @end
%%%-------------------------------------------------------------------

-module(bench_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("otters/include/otters.hrl").

-export([all/0,
         bench_old_filter/1, bench_new_filter/1,
         bench_old_filter_large/1, bench_new_filter_large/1,
         bench_encoding/1]).

all() ->
    [
     bench_encoding,
     bench_old_filter, bench_new_filter,
     bench_old_filter_large, bench_new_filter_large
    ].

bench_old_filter(_) ->
    %% Set the filter
    %%application:ensure_all_started(otters),
    Filter = [
              {
                %% Condition
                [
                 {greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}
                ],
                %% Action
                [
                 {snapshot_count, [long_radius_request], []},
                 send_to_zipkin
                ]
              },

              {
                %% Condition counts all requests with name and result
                [{present, <<"final_result">>}],
                %% Action
                [{snapshot_count, [request],
                  [<<"otters_span_name">>, <<"final_result">>]}]
              }
             ],
    otters_config:write(filter_rules, Filter),
    ol:clear(),
    run(fun run_spans_f/1).

bench_old_filter_large(_) ->
    %% Set the filter
    %%application:ensure_all_started(otters),
    Filter = [
              {
                %% Condition
                [
                 {greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}
                ],
                %% Action
                [
                 {snapshot_count, [long_radius_request], []},
                 send_to_zipkin
                ]
              },
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},
              {[{greater, <<"otters_span_duration">>, 5000000},
                 {value, <<"otters_span_name">>, <<"radius request">>}],
                [{snapshot_count, [<<"i">>], []}]},

              {
                %% Condition counts all requests with name and result
                [{present, <<"final_result">>}],
                %% Action
                [{snapshot_count, [request],
                  [<<"otters_span_name">>, <<"final_result">>]}]
              }
             ],
    otters_config:write(filter_rules, Filter),
    ol:clear(),
    run(fun run_spans_f/1).

bench_new_filter_large(_) ->
    %% Set the filter
    %%application:ensure_all_started(otters),
    Filter = "%% If our span takes less then 5s skip the rest of the rules\n"
        "slow_spans(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans() ->\n"
        "    count('long_radius_request').\n"
        "%% Send\n"
        "slow_spans() ->\n"
        "    send.\n"

        "slow_spans1(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans1(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans1() ->\n"
        "    count('i').\n"

        "slow_spans2(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans2(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans2() ->\n"
        "    count('i').\n"

        "slow_spans3(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans3(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans3() ->\n"
        "    count('i').\n"

        "slow_spans4(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans4(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans4() ->\n"
        "    count('i').\n"

        "slow_spans5(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans5(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans5() ->\n"
        "    count('i').\n"

        "slow_spans6(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans6(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans6() ->\n"
        "    count('i').\n"

        "slow_spans7(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans7(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans7() ->\n"
        "    count('i').\n"

        "slow_spans8(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans8(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans8() ->\n"
        "    count('i').\n"

        "slow_spans9(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans9(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans9() ->\n"
        "    count('i').\n"

        "%% Count them all\n"
        "count(final_result) ->\n"
        "    count('request', otter_span_name, final_result).\n" ,
    ol:compile(Filter),
    run(fun run_spans/1).

bench_new_filter(_) ->
    %% Set the filter
    %%application:ensure_all_started(otters),
    Filter = "%% If our span takes less then 5s skip the rest of the rules\n"
        "slow_spans(otters_span_duration > 5000000) ->\n"
        "    continue.\n"
        "%% Skip requests that are not radius requests\n"
        "slow_spans(otters_span_name == 'radius request') ->\n"
        "    continue.\n"
        "%% Count\n"
        "slow_spans() ->\n"
        "    count('long_radius_request').\n"
        "%% Send\n"
        "slow_spans() ->\n"
        "    send.\n"
        "%% Count them all\n"
        "count(final_result) ->\n"
        "    count('request', otter_span_name, final_result).\n" ,
    ol:compile(Filter),
    run(fun run_spans/1).

run(Fn) ->
    {module, otters_conn_zipkin} =
        dynamic_compile:load_from_string(
          "-module(otters_conn_zipkin).\n"
          "-export([store_span/1]).\n"
          "store_span(_) -> ok.\n"),
    {module, otters_snapshot_count} =
        dynamic_compile:load_from_string(
          "-module(otters_snapshot_count).\n"
          "-export([snapshot/2]).\n"
          "snapshot(_, _) -> ok.\n"),
    Count = 100000,
    Spans = mk_spans(),
    Seq = lists:seq(1, Count),
    {T, _} = timer:tc(fun () ->
                              lists:foreach(fun(_) ->
                                                    Fn(Spans)
                                            end, Seq)
                      end),
    io:format(user, "~.2f microseconds / span.~n",
              [T / (Count * length(Spans))]),
    %% We log 2 out of 3 messages so we need to multiply this
    %% by 2
    %%?assertEqual(2 * Count, LogCount),
    %% For long count we only have one of the spans
    %% matching so we count then only once
    %%?assertEqual(Count, LogLongCount),
    %%?assertEqual(Count, SendCount).
    ok.

%%% Original
%%% Encoding: 422.18 microseconds / span.
%%% Decoding: 140.12 microseconds / span.
%%% bench_SUITE ==> bench_encoding: OK
%%% New
%%% Encoding: 9.73 microseconds / span.
%%% Decoding: 127.38 microseconds / span.
%%% bench_SUITE ==> bench_encoding: OK
bench_encoding(_) ->
    Count = 10000,
    Spans = mk_spans(),
    Spans1 = [Spans || _ <- lists:seq(1, Count)],
    Spans2 = lists:flatten(Spans1),
    Total = length(Spans2),
    {To, _Encoded} = timer:tc(otters_conn_zipkin, encode_spans, [Spans2]),
    {Te, Encoded} = timer:tc(otters_zipkin_encoder, encode, [Spans2]),

    io:format(user, "Encoding: ~.2f microseconds / span.~n",
              [Te / Total]),
    io:format(user, "Encoding(Old): ~.2f microseconds / span.~n",
              [To / Total]),
    {Td, _} = timer:tc(otters_conn_zipkin, decode_spans, [Encoded]),
    io:format(user, "Decoding: ~.2f microseconds / span.~n",
              [Td / Total]),
    ok.

c_l(N) ->
    receive
        {get, P} ->
            P ! {n, N};
        inc ->
            c_l(N + 1)
    end.

run_spans_f([]) ->
    ok;
run_spans_f([S | R]) ->
    otters_filter:span(S),
    run_spans_f(R).

run_spans([]) ->
    ok;
run_spans([S | R]) ->
    ol:span(S),
    run_spans(R).

mk_spans() ->
    S = mk_span(),
    Tags = S#span.tags,
    Tags1 = Tags#{<<"final_result">> => {<<"yay!">>, undefined}},
    [
     %% Matches no rules
     S,
     %% Matches the count rule
     S#span{tags = Tags1},
     %% Matches both rules
     S#span{duration = 5000001, name = <<"radius request">>, tags = Tags1}
    ].
    %%[S].


mk_span() ->
    #span{
       id        = 0,
       timestamp = 0,
       trace_id  = 0,
       duration = 100,
       name = <<"other request">>,
       tags = #{
         <<"1">> => {1, undefined},
         <<"2">> => {<<"hello">>, undefined},
         <<"3">> => {1, default},
         <<"4">> => {1, undefined},
         <<"5">> => {<<"hello">>, default},
         <<"6">> => {1, {<<"test">>, {127, 0, 0, 1}, 1}},
         <<"7">> => {1, undefined},
         <<"8">> => {<<"hello">>, undefined},
         <<"9">> => {1, default},
         <<"10">> => {1, {<<"test">>, {127, 0, 0, 1}, 1}}
        },
       logs = [
               {1, <<"test">>, default},
               {2, <<"bla">>, undefined},
               {3, <<"blubber">>, default},
               {4, <<"hello">>, {<<"test">>, {127, 0, 0, 1}, 1}},
               {5, <<"world">>}
              ]
      }.
