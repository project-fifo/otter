-module(bench_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("otters/include/otters.hrl").

-export([all/0, bench_old_filter/1, bench_new_filter/1]).

all() ->
    [bench_new_filter, bench_old_filter].

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
    run().

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
    run().

run() ->
    Send = spawn(fun() -> c_l(0) end),
    Log = spawn(fun() -> c_l(0) end),
    LogLong = spawn(fun() -> c_l(0) end),
    meck:new(otters_conn_zipkin, [passthrough]),
    meck:expect(otters_conn_zipkin, store_span,
                fun (_) ->
                        Send ! inc
                end),
    meck:new(otters_snapshot_count, [passthrough]),
    meck:expect(otters_snapshot_count, snapshot,
                fun ([<<"long_radius_request">>], _) ->
                        LogLong ! inc;
                    ([long_radius_request], _) ->
                        LogLong ! inc;
                    (_, _) ->
                        Log ! inc
                end),
    Count = 100000,
    Spans = mk_spans(),
    Seq = lists:seq(1, Count),
    {T, _} = timer:tc(fun () ->
                              lists:foreach(fun(_) ->
                                                    run_spans(Spans)
                                            end, Seq)
                      end),
    Log ! {get, self()},
    LogCount = receive
                   {n, LogCountX} ->
                       LogCountX
               end,
    LogLong ! {get, self()},
    LogLongCount = receive
                   {n, LogLongCountX} ->
                       LogLongCountX
               end,
    Send ! {get, self()},
    SendCount = receive
                    {n, SendCountX} ->
                        SendCountX
                end,
    meck:unload(otters_conn_zipkin),
    meck:unload(otters_snapshot_count),
    io:format(user, "~.2f microseconds / span.~n", [T / (Count * length(Spans))]),
    io:format(user, "  Logged a total of ~p spans.~n", [LogCount + LogLongCount]),
    io:format(user, "  Send a total of ~p spans.~n", [SendCount]),
    %% We log 2 out of 3 messages so we need to multiply this
    %% by 2
    ?assertEqual(2 * Count, LogCount),
    %% For long count we only have one of the spans
    %% matching so we count then only once
    ?assertEqual(Count, LogLongCount),
    ?assertEqual(Count, SendCount).


c_l(N) ->
    receive
        {get, P} ->
            P ! {n, N};
        inc ->
            c_l(N + 1)
    end.

run_spans([]) ->
    ok;
run_spans([S | R]) ->
    otters_filter:span(S),
    run_spans(R).

mk_spans() ->
    S = mk_span(),
    Tags = S#span.tags,
    Tags1 = Tags#{<<"final_result">> => <<"yay!">>},
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
       duration = 100,
       name = <<"other request">>,
       tags = #{
         <<"1">> => 1,
         <<"2">> => 1,
         <<"3">> => 1,
         <<"4">> => 1,
         <<"5">> => 1,
         <<"6">> => 1,
         <<"7">> => 1,
         <<"8">> => 1,
         <<"9">> => 1,
         <<"10">> => 1
        }
      }.
