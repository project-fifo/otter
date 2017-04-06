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
%%% otters API module
%%% @end
%%%-------------------------------------------------------------------

-module(otters).
-include("otters.hrl").

-export([start/1, start/2, start/3,
         tag/3, tag/4,
         log/2, log/3,
         finish/1,
         ids/1
        ]).

-export_type([info/0, service/0, trace_id/0, span_id/0,
              span/0, maybe_span/0]).

-type time_us()    :: non_neg_integer().            % timestamp in microseconds
-type info()       :: binary() | iolist() | atom() | integer().
-type ip4()        :: {0..255, 0..255, 0..255, 0..255}.
-type service()    :: binary() | list() | default |
                      {binary() | list(), ip4(), integer()}.
-type trace_id()   :: integer().
-type span_id()    :: integer().
-type span()       :: #span{}.
-type maybe_span() :: span() | undefined.




%% ====================  SPAN function API  ======================
%% This API functions with passing around the Span in the function calls
%% All of them return a Span structure (erlang map).

-spec start(info()) -> span().
start(Name) ->
    start(Name, otters_lib:id()).


-spec start(info(), integer() | span()) -> span().
start(Name, TraceId)
  when is_integer(TraceId) ->
    start(Name, TraceId, undefined);
start(Name, ParentSpan)
  when is_record(ParentSpan, span) ->
    {TraceId, ParentId} = ids(ParentSpan),
    start(Name, TraceId, ParentId).

-spec start(info(), integer(), integer()| undefined) ->
                   span().

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

-spec tag(maybe_span(), info(), info()) -> maybe_span().
tag(undefined, _Key, _Value) ->
    undefined;
tag(Span, Key, Value)
  when is_record(Span, span) ->
    Tags = Span#span.tags,
    Span#span{
      tags = lists:keystore(Key, 1, Tags, {Key, Value})
     }.

-spec tag(maybe_span(), info(), info(), service()) -> maybe_span().

tag(undefined, _Key, _Value, _Service) ->
    undefined;
tag(Span, Key, Value, Service)
  when is_record(Span, span) ->
    Tags = Span#span.tags,
    Span#span{
      tags = lists:keystore(Key, 1, Tags, {Key, Value, Service})
     }.


-spec log(maybe_span(), info()) -> maybe_span().
log(undefined, _Text) ->
    undefined;
log(Span, Text)
  when is_record(Span, span) ->
    Logs = Span#span.logs,
    Span#span{
      logs = [{otters_lib:timestamp(), Text} | Logs]
     }.

-spec log(maybe_span(), info(), service()) -> maybe_span().
log(undefined, _Text, _Service) ->
    undefined;
log(Span, Text, Service)
  when is_record(Span, span) ->
    Logs = Span#span.logs,
    Span#span{
      logs = [{otters_lib:timestamp(), Text, Service} | Logs]
     }.

-spec finish(maybe_span()) -> ok.
finish(undefined) ->
    undefined;
finish(Span)
  when is_record(Span, span) ->
    Start = Span#span.timestamp,
    Logs = Span#span.logs,
    otters_filter:span(
      Span#span{
        duration = otters_lib:timestamp() - Start,
        logs = lists:reverse(Logs)
       }),
    ok.
-spec ids(maybe_span()) -> {trace_id(), span_id()} | undefined.
ids(undefined) ->
    undefined;
ids(Span)
  when is_record(Span, span) ->
    #span{trace_id = TraceId, id = Id} = Span,
    {TraceId, Id}.

%% ========================  Snap/Count API  =========================
%% When span_end/1 or span_pend/0 is called then the completed span is
%% passed to a configurable filter. The filter can check the Span tags
%% as well as the name and duration of the span and use the information
%% to decide to send the Span to the trace collector (Zipkin supported)
%% and/or increase counters based on values of the tags and store the
%% last Span for the counters. This latter is particularly useful for
%% troubleshooting e.g. error events when increase of the corresponding
%% counter is noticed. These snapshots (referred as Snap) and counters
%% can be retrieved, managed with this API

%% -spec counter_list() -> [{list(), integer()}].
%% counter_list() ->
%%     otters_snapshot_count:list_counts().

%% -spec counter_snapshot(list()) -> term().
%% counter_snapshot(Key) ->
%%     otters_snapshot_count:get_snap(Key).

%% -spec counter_delete(list()) -> ok.
%% counter_delete(Key) ->
%%     otters_snapshot_count:delete_counter(Key).

%% -spec counter_delete_all() -> ok.
%% counter_delete_all() ->
%%     otters_snapshot_count:delete_all_counters().
