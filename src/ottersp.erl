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
%%% This API uses the process dictionary to collect span information
%%% and can be used when all span tags an events happen in the same
%%% request handling process.
%%% @end
%%%-------------------------------------------------------------------

-module(ottersp).

-export([start/1, start/2, start/3,
         start_child/2,
         tag/2, tag/3,
         log/1, log/2,
         finish/0,
         ids/0,
         get_span/0
        ]).

-define(KEY, otters_span_information).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given name and a generated trace id.
%% @end
%%--------------------------------------------------------------------
-spec start(otters:info()) -> ok.
start(Name) ->
    put(?KEY, otters:start(Name)).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given Trace ID.
%% @end
%%--------------------------------------------------------------------
-spec start(otters:info(), otters:trace_id()) -> ok.
start(Name, TraceId) ->
    put(?KEY, otters:start(Name, TraceId)).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span with a given Trace ID and Parent ID.
%% @end
%%--------------------------------------------------------------------
-spec start(otters:info(), otters:trace_id(), otters:span_id()) -> ok.
start(Name, TraceId, ParentId) ->
    put(?KEY, otters:start(Name, TraceId, ParentId)).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new span as a child of a existing span, using the parents
%% Trace ID and setting the childs parent to the parents Span ID
%% @end
%%--------------------------------------------------------------------
-spec start_child(otters:info(), otters:maybe_span()) -> otters:maybe_span().
start_child(Name, ParentSpan) ->
    put(?KEY, otters:start_child(Name, ParentSpan)).

%%--------------------------------------------------------------------
%% @doc
%% Adds a tag to the span, possibly overwriting the existing value.
%% @end
%%--------------------------------------------------------------------
-spec tag(otters:info(), otters:info()) -> ok.
tag(Key, Value) ->
    Span = get(?KEY),
    put(?KEY, otters:tag(Span, Key, Value)),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Adds a tag to the span, possibly overwriting the existing value.
%% @end
%%--------------------------------------------------------------------
-spec tag(otters:info(), otters:info(), otters:service()) -> ok.
tag(Key, Value, Service) ->
    Span = get(?KEY),
    put(?KEY, otters:tag(Span, Key, Value, Service)),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Adds a tag to the span, possibly overwriting the existing value.
%% @end
%%--------------------------------------------------------------------
-spec log(otters:info()) -> ok.
log(Text) ->
    Span = get(?KEY),
    put(?KEY, otters:log(Span, Text)),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Adds the log to a span with a given service, possibly overwriting
%% the existing value.
%% @end
%%--------------------------------------------------------------------
-spec log(otters:info(), otters:service()) -> ok.
log(Text, Service) ->
    Span = get(?KEY),
    put(?KEY, otters:log(Span, Text, Service)),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Ends the span and prepares queues it to be dispatched to the trace
%% server.
%% @end
%%--------------------------------------------------------------------
-spec finish() -> ok.
finish() ->
    Span = get(?KEY),
    otters:finish(Span),
    put(?KEY, undefined).

%% This call can be used to retrieve the IDs from the calling process
%% e.g. when you have a gen_server and you get an API function call
%% (which is in the context of the calling process) then calling pget_id()
%% returns a {TraceId, SpanId} that is stored with the process API calls
%% above for the calling process, so they can be used in the handling of
%% the call

%%--------------------------------------------------------------------
%% @doc
%% Retrives the Trace ID and the Span ID from a span.
%% @end
%%--------------------------------------------------------------------
-spec ids() -> {otters:trace_id(), otters:span_id()} | undefined.
ids() ->
    Span = get(?KEY),
    otters:ids(Span).

-spec get_span() -> otters:maybe_span().
get_span() ->
    get(?KEY).
