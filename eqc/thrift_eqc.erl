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

span(0) ->
    new_span();

span(Size) ->
    ?LAZY(oneof(
            [
             {call, otters, log, [span(Size -1), binary()]},
             {call, otters, log, [span(Size -1), binary(), binary()]},
             {call, otters, tag, [span(Size -1), binary(), binary()]},
             {call, otters, tag, [span(Size -1), binary(), binary(), binary()]}
            ])).

span() ->
    ?SIZED(Size, span(Size)).

prop_encode_decode() ->
    ?FORALL(Raw, list(span()),
            begin
                Spans = [eval(S) || S <- Raw],
                Encoded = otters_conn_zipkin:encode_spans(Spans),
                Decoded = otters_conn_zipkin:decode_spans(Encoded),
                Cleaned = [cleanup(S) || S <- Decoded],
                ?WHENFAIL(
                   io:format(user,
                             "~p -> ~p~n",
                             [Spans, Cleaned]),
                   Spans =:= Cleaned)
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
    maps:map(fun (_, {V, {S, {127,0,0,1}, 0}}) ->
                     {V, S};
                 (_, V) ->
                     V
             end, maps:remove(<<"lc">>, Tags)).

clean_logs(Logs) ->
    [clean_log(L) || L <- Logs].


clean_log({T,V,{S,_,_}}) ->
    {T, V, S};
clean_log(O) ->
    O.