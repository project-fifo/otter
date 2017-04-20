%%%-------------------------------------------------------------------
%%% Licensed to the Apache Software Foundation (ASF) under one
%%% or more contributor license agreements.  See the NOTICE file
%%% distributed with this work for additional information
%%% regarding copyright ownership.  The ASF licenses this file
%%% to you under the Apache License, Version 2.0 (the
%%% "License"); you may not use this file except in compliance
%%% with the License.  You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%
%%% @doc
%%% otters API module. Functions have no effect when
%%% <em>undefined</em> is passed as a spawn.
%%%
%%% This API functions with passing around the Span in the function calls
%%% All of them return a Span structure.
%%% @end
%%%-------------------------------------------------------------------

-module(otters).
-include("otters.hrl").

-export([start/1, start/2, start/3,
         start_child/2,
         tag/3, tag/4,
         log/2, log/3,
         finish/1,
         ids/1
        ]).

-export_type([info/0, service/0, trace_id/0, span_id/0,
              span/0, maybe_span/0, tags/0]).

-type info()       :: binary() | iolist() | atom() | integer().
-type ip4()        :: {0..255, 0..255, 0..255, 0..255}.
-type service()    :: binary() | list() | default | undefined |
                      {binary() | list(), ip4(), integer()}.
-type trace_id()   :: non_neg_integer().
-type span_id()    :: non_neg_integer().
-type tag()        :: {info(), service()}
                    | binary() | string() | atom().
-type tags()       :: #{binary() => tag()}.
-type span()       :: #span{}.
-type maybe_span() :: span() | undefined.

%% timestamp in microseconds
-type time_us()    :: non_neg_integer().


%% ====================  SPAN function API  ======================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given name and a generated trace id.
%% @end
%%--------------------------------------------------------------------
-spec start(info()) -> span().
start(Name) ->
    start(Name, otters_lib:id()).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given Trace ID.
%% @end
%%--------------------------------------------------------------------
-spec start(info(), integer() | undefined) ->
                   maybe_span().
start(_Name, undefined) ->
    undefined;
start(Name, TraceId)
  when is_integer(TraceId) ->
    start(Name, TraceId, undefined).


%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given Trace ID and Parent ID.
%% @end
%%--------------------------------------------------------------------
-spec start(info(), integer() | undefined, integer() | undefined) ->
                   maybe_span().
start(_Name, undefined, undefined) ->
    undefined;
start(Name, TraceId, ParentId)
  when is_integer(TraceId), (is_integer(ParentId) orelse
                             ParentId =:= undefined) ->
    #span{
       timestamp = otters_lib:timestamp(),
       trace_id = TraceId,
       id = otters_lib:id(),
       parent_id = ParentId,
       name = Name
      }.

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span as a child of a existing span, using the parents
%% Trace ID and setting the childs parent to the parents Span ID
%% @end
%%--------------------------------------------------------------------
-spec start_child(info(), maybe_span() |
                  {TraceID::trace_id(), SpanID::span_id()}) -> maybe_span().
start_child(_Name, undefined) ->
    undefined;
start_child(Name, #span{trace_id = TraceId, parent_id = ParentId}) ->
    start(Name, TraceId, ParentId);
start_child(Name, {TraceId, ParentId}) ->
    start(Name, TraceId, ParentId).


%%--------------------------------------------------------------------
%% @doc
%% Adds a tag to a span, possibly overwriting the existing value.
%% @end
%%--------------------------------------------------------------------
-spec tag(maybe_span(), info(), info()) -> maybe_span().
tag(undefined, _Key, _Value) ->
    undefined;
tag(Span = #span{}, Key, Value) ->
    tag(Span, Key, Value, undefined).


%%--------------------------------------------------------------------
%% @doc
%% Adds a tag to a span with a given service, possibly overwriting
%% the existing value.
%% @end
%%--------------------------------------------------------------------
-spec tag(maybe_span(), info(), info(), service() | undefined) -> maybe_span().
tag(undefined, _Key, _Value, _Service) ->
    undefined;
tag(Span= #span{}, Key, Value, Service) ->
    KeyBin = otters_lib:to_bin(Key),
    Span#span{
      tags = maps:put(KeyBin, {v(Value), Service}, Span#span.tags)
     }.

%%--------------------------------------------------------------------
%% @doc
%% Adds a tag to a span, possibly overwriting the existing value.
%% @end
%%--------------------------------------------------------------------
-spec log(maybe_span(), info()) -> maybe_span().
log(undefined, _Text) ->
    undefined;
log(Span = #span{logs = Logs}, Text) ->
    Span#span{
      logs = [{otters_lib:timestamp(), otters_lib:to_bin(Text), undefined} | Logs]
     }.

%%--------------------------------------------------------------------
%% @doc
%% Adds a log to a span with a given service, possibly overwriting
%% the existing value.
%% @end
%%--------------------------------------------------------------------
-spec log(maybe_span(), info(), service()) -> maybe_span().
log(undefined, _Text, _Service) ->
    undefined;
log(Span = #span{logs = Logs}, Text, Service) ->
    Span#span{
      logs = [{otters_lib:timestamp(), otters_lib:to_bin(Text), Service} | Logs]
     }.

%%--------------------------------------------------------------------
%% @doc
%% Ends a span and prepares queues it to be dispatched to the trace
%% server.
%% @end
%%--------------------------------------------------------------------
-spec finish(maybe_span()) -> ok.
finish(undefined) ->
    undefined;
finish(Span = #span{logs = Logs, timestamp = Start}) ->
    ol:span(
      Span#span{
        duration = otters_lib:timestamp() - Start,
        logs = lists:reverse(Logs)
       }),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Retrives the Trace ID and the Span ID from a span.
%% @end
%%--------------------------------------------------------------------
-spec ids(maybe_span()) -> {TraceID::trace_id(), SpanID::span_id()} | undefined.
ids(undefined) ->
    undefined;
ids(#span{trace_id = TraceId, id = Id}) ->
    {TraceId, Id}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

v(I) when is_integer(I) ->
    I;
v(B) when is_binary(B) ->
    B;
v(O) ->
    otters_lib:to_bin(O).
