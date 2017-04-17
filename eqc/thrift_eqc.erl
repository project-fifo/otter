-module(thrift_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("otters/include/otters.hrl").
-compile(export_all).

pos_int() ->
    ?SUCHTHAT(N, int(), N > 0).


name() ->
    <<"name">>.


parent_id() ->
    oneof([pos_int(), undefined]).

trace_id() ->
    int().

timestap() ->
    pos_int().

duration() ->
    int().
tags() ->
    #{}.

logs() ->
    [].


new_span() ->
    #span{
       id        = trace_id(),
       timestamp = timestap(),
       trace_id  = trace_id(),
       name      = name(),
       parent_id = parent_id(),
       duration  = duration(),
       tags      = tags(),
       logs      = logs()
      }.

ip() ->
    {127, choose(0, 254), choose(0, 254), choose(1, 254)}.

service() ->
    oneof([default,
           {binary(), ip(), pos_int()},
           binary()]).

log() ->
    oneof([{timestap(), binary()},
           {timestap(), binary(), service()}]).

tag() ->
    {binary(), {binary(), service()}}.
span(0) ->
    new_span();

span(Size) ->
    ?LAZY(oneof(
            [
             {call, otters, log, [span(Size -1), binary()]},
             {call, otters, log, [span(Size -1), binary(), service()]},
             {call, otters, tag, [span(Size -1), binary(), binary()]},
             {call, otters, tag, [span(Size -1), binary(), binary(), service()]}
            ])).

span() ->
    ?SIZED(Size, span(Size)).

prop_encode_decode() ->
    ?FORALL(Raw, list(span()),
            begin
                Spans = [eval(S) || S <- Raw],
                Encoded = otters_zipkin_encoder:encode(Spans),
                Decoded = otters_conn_zipkin:decode_spans(Encoded),
                Cleaned = [cleanup(S) || S <- Decoded],
                CleanedIn = [cleanup(S) || S <- Spans],
                ?WHENFAIL(
                   io:format(user,
                             "~p -> ~p~n",
                             [CleanedIn, Cleaned]),
                   CleanedIn =:= Cleaned)
            end).

prop_encode_old() ->
    ?FORALL(Raw, list(span()),
            begin
                Spans = [eval(S) || S <- Raw],
                Encoded = otters_zipkin_encoder:encode(Spans),
                EncodedOld = otters_conn_zipkin:encode_spans_old(Spans),
                ?WHENFAIL(
                   io:format(user,
                             "~p =>\n~p =/=\n~p~n",
                             [Spans, Encoded, EncodedOld]),
                   Encoded =:= EncodedOld)
            end).


prop_cmp_log() ->
    ?FORALL(Log, log(),
            begin
                {_DfltTag, DfltSrv, UndefSrvLog, _UndefSrvTag} =
                    otters_zipkin_encoder:defaults(),
                Encoded = otters_zipkin_encoder:encode_log(Log, DfltSrv,
                                                           UndefSrvLog),
                Raw = otters_conn_zipkin:log_to_annotation(Log),
                EncodedOld = otters_conn_zipkin:encode({struct, Raw}),
                ?WHENFAIL(
                   io:format(user,
                             "~p =>\n~p =/=\n~p~n",
                             [Log, Encoded, EncodedOld]),
                   Encoded =:= EncodedOld)
            end).

prop_cmp_tag() ->
    ?FORALL(Tag, tag(),
            begin
                {_DfltTag, DfltSrv, _UndefSrvLog, UndefSrvTag} =
                    otters_zipkin_encoder:defaults(),
                Encoded = otters_zipkin_encoder:encode_tag(Tag, DfltSrv,
                                                           UndefSrvTag),
                Raw = otters_conn_zipkin:tag_to_binary_annotation(Tag),
                EncodedOld = otters_conn_zipkin:encode({struct, Raw}),
                ?WHENFAIL(
                   io:format(user,
                             "~p =>\n~p =/=\n~p~n",
                             [Tag, Encoded, EncodedOld]),
                   Encoded =:= EncodedOld)
            end).

cleanup(S = #span{
               tags = Tags,
               logs = Logs
              }) ->
    S#span{
      tags = clean_tags(Tags),
      logs = clean_logs(Logs)
     }.

clean_tags(Tags) ->
    maps:map(fun (_, {V, {<<"otters_test">>, {127,0,0,1}, 0}}) ->
                     {V, default};
                 (_, {V, {S, {127,0,0,1}, 0}}) ->
                     {V, S};
                 (_, V) ->
                     V
             end, maps:remove(<<"lc">>, Tags)).

clean_logs(Logs) ->
    [clean_log(L) || L <- Logs].


clean_log({T, V, {<<"otters_test">>, {127,0,0,1}, 0}}) ->
    {T, V, default};
clean_log({T, V, {S, {127,0,0,1}, 0}}) ->
    {T, V, S};
clean_log({T, V}) ->
    {T, V, default};
clean_log(O) ->
    O.
