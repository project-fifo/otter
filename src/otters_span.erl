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
%%%-------------------------------------------------------------------

-module(otters_span).
-compile(export_all).
-include("otters.hrl").

%% ====================  SPAN function API  ======================
%% This API functions with passing around the Span in the function calls

fstart(Name) ->
    fstart(Name, otters_lib:id()).
fstart(Name, TraceId) ->
    fstart(Name, TraceId, undefined).
fstart(Name, TraceId, ParentId) ->
    #span{
        timestamp = otters_lib:timestamp(),
        trace_id = TraceId,
        id = otters_lib:id(),
        parent_id = ParentId,
        name = Name
    }.

ftag(Span, Key, Value) ->
    Tags = Span#span.tags,
    Span#span{
        tags = lists:keystore(Key, 1, Tags, {Key, Value})
    }.

ftag(Span, Key, Value, Service) ->
    Tags = Span#span.tags,
    Span#span{
        tags = lists:keystore(Key, 1, Tags, {Key, Value, Service})
    }.

flog(Span, Text) ->
    Logs = Span#span.logs,
    Span#span{
        logs = [{otters_lib:timestamp(), Text} | Logs]
    }.

flog(Span, Text, Service) ->
    Logs = Span#span.logs,
    Span#span{
        logs = [{otters_lib:timestamp(), Text, Service} | Logs]
    }.

fend(Span) ->
    Start = Span#span.timestamp,
    Logs = Span#span.logs,
    otters_filter:span(Span#span{
        duration = otters_lib:timestamp() - Start,
        logs = lists:reverse(Logs)
    }),
    ok.

fget_ids(Span) ->
    #span{trace_id = TraceId, id = Id} = Span,
    {TraceId, Id}.

%% ====================  SPAN process API  ======================
%% This API uses the process dictionary to collect span information
%% and can be used when all span tags an events happen in the same
%% request handling process.

pstart(Name) ->
    pstart(Name, otters_lib:id()).

pstart(Name, TraceId) ->
    pstart(Name, TraceId, undefined).

pstart(Name, TraceId, ParentId) ->
    put(otters_span_information, fstart(Name, TraceId, ParentId)),
    ok.

ptag(Key, Value) ->
    Span = get(otters_span_information),
    put(otters_span_information, ftag(Span, Key, Value)),
    ok.

ptag(Key, Value, Service) ->
    Span = get(otters_span_information),
    put(otters_span_information, ftag(Span, Key, Value, Service)),
    ok.

plog(Text) ->
    Span = get(otters_span_information),
    put(otters_span_information, flog(Span, Text)),
    ok.

plog(Text, Service) ->
    Span = get(otters_span_information),
    put(otters_span_information, flog(Span, Text, Service)),
    ok.

pend() ->
    Span = get(otters_span_information),
    fend(Span).

%% This call can be used to retrieve the IDs from the calling process
%% e.g. when you have a gen_server and you get an API function call
%% (which is in the context of the calling process) then calling pget_id()
%% returns a {TraceId, SpanId} that is stored with the process API calls
%% above for the calling process, so they can be used in the handling of
%% the call
pget_ids() ->
    #span{trace_id = TraceId, id = Id} = get(otters_span_information),
    {TraceId, Id}.

pget_span() ->
    get(otters_span_information).
