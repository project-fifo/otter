-module(otter_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, ptest/1, ftest/1, handle_span/1]).

all() ->
    [ftest, ptest].

ptest(_Config) ->
    application:ensure_all_started(otters),
    application:set_env(otters, zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),

    meck:new(ibrowse, [passthrough]),
    meck:expect(ibrowse, send_req, fun send_req/4),

    ottersp:start("test_span"),
    ottersp:log("started"),
    ottersp:log("α =:= ω"),
    ottersp:tag("result", "ok"),
    ottersp:finish(),
    timer:sleep(200),
    ?assert(meck:validate(ibrowse)),
    meck:unload(ibrowse).

ftest(_Config) ->
    application:ensure_all_started(otters),
    application:set_env(otters, zipkin_collector_uri, "http://127.0.0.1:19411/api/v1/spans"),
    application:set_env(otters, server_zipkin_callback, {?MODULE, handle_span}),
    ets:new(test_span_collector, [named_table, public, {keypos, 2}]),

    meck:new(ibrowse, [passthrough]),
    meck:expect(ibrowse, send_req, fun send_req/4),

    S1 = otters:start("test_span"),
    S2 = otters:log(S1, "started"),
    S3 = otters:tag(S2, "result", "ok"),
    S4 = otters:log(S3, "α =:= ω"),
    S5 = otters:log(S4, 123456),
    S6 = otters:log(S5, 'this is a atom'),
    S7 = otters:log(S6, io_lib:format("int: ~w, float: ~f, hex: ~.16B, Span: ~p",
					  [1, 1.0, 1, S6])),
    S8 = otters:log(S7, S7),
    S9 = otters:log(S8, fun() -> "result of function" end),
    otters:finish(S9),
    timer:sleep(200),
    ?assert(meck:validate(ibrowse)),
    meck:unload(ibrowse).


send_req(_ZipkinURL,
         [{"content-type", "application/x-thrift"}],
         post, Data) ->
    1 = length(otters_conn_zipkin:decode_spans(Data)),
    {ok, {{stuff, 200, stuff}, stuff, stuff}}.

handle_span(Span)  ->
    ets:insert(test_collector, Span).
