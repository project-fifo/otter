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

%% @doc This module facilitates encoding/decoding of thrift data lists encoded
%% with the binary protocol, suitable for sending/receiving spans
%% on the zipkin interface.

%% Sending is async

-module(otters_conn_zipkin).
-compile(export_all).
-include("otters.hrl").

-define(T_I16,     6).
-define(T_I32,     8).
-define(T_I64,    10).
-define(T_STRING, 11).
-define(T_STRUCT, 12).
-define(T_LIST,   15).

sup_init() ->
    [
     ets:new(
       Tab,
       [named_table, public | TableProps ]
      ) ||
        {Tab, TableProps} <-
            [
             {otters_zipkin_buffer1, [{write_concurrency, true}, {keypos, 2}]},
             {otters_zipkin_buffer2, [{write_concurrency, true}, {keypos, 2}]},
             {otters_zipkin_status,  [{read_concurrency, true}]}
            ]
    ],
    ets:insert(otters_zipkin_status, {current_buffer, otters_zipkin_buffer1}),
    SendInterval = otters_config:read(zipkin_batch_interval_ms, 100),
    timer:apply_interval(SendInterval, ?MODULE, send_buffer, []).

store_span(Span) ->
    [{_, Buffer}] = ets:lookup(otters_zipkin_status, current_buffer),
    ets:insert(Buffer, Span).

send_buffer() ->
    [{_, Buffer}] = ets:lookup(otters_zipkin_status, current_buffer),
    NewBuffer = case Buffer of
                    otters_zipkin_buffer1 ->
                        otters_zipkin_buffer2;
                    otters_zipkin_buffer2 ->
                        otters_zipkin_buffer1
                end,
    ets:insert(otters_zipkin_status, {current_buffer, NewBuffer}),
    case ets:tab2list(Buffer) of
        [] ->
            ok;
        Spans ->
            ets:delete_all_objects(Buffer),
            case send_batch_to_zipkin(Spans) of
                {ok, 202} ->
                    otters_snapshot_count:snapshot(
                      [?MODULE, send_buffer, ok],
                      [{spans, length(Spans)}]);
                Error ->
                    otters_snapshot_count:snapshot(
                      [?MODULE, send_buffer, failed],
                      [
                       {spans, length(Spans)},
                       {error, Error}
                      ])
            end
    end.

send_batch_to_zipkin(Spans) ->
    {ok, ZipkinURL} = otters_config:read(zipkin_collector_uri),
    send_batch_to_zipkin(ZipkinURL, Spans).

send_batch_to_zipkin(ZipkinURL, Spans) ->
    Data = encode_spans(Spans),
    send_spans_http(ZipkinURL, Data).

send_spans_http(ZipkinURL, Data) ->
    send_spans_http(application:get_env(otters, http_client, ibrowse),
                    ZipkinURL, Data).

send_spans_http(ibrowse, ZipkinURL, Data) ->
    case ibrowse:send_req(
           ZipkinURL,
           [{"content-type", "application/x-thrift"}],
           post,
           Data
          ) of
        {ok, SCode, _, _} ->
            {ok, list_to_integer(SCode)};
        Err ->
            Err
    end;
send_spans_http(httpc, ZipkinURL, Data) ->
    case httpc:request(post, {ZipkinURL, [], "application/x-thrift", Data},
                       [], []) of
        {ok, {{_, SCode, _}, _, _}} ->
            {ok, SCode};
        Err ->
            Err
    end.

encode_spans(Spans) ->
    Size = length(Spans),
    AddDfltToLog = otters_config:read(zipkin_add_default_service_to_logs,
                                      false),
    AddDfltToTag = otters_config:read(zipkin_add_default_service_to_tags,
                                      false),
    DfltSrv = <<?T_STRUCT, 4:16, (host_to_struct(default))/binary>>,
    UndefSrvLog = case AddDfltToLog of
                      true ->
                          DfltSrv;
                      _ ->
                           <<>>
                   end,
    UndefSrvTag = case AddDfltToTag of
                       true ->
                          DfltSrv;
                       _ ->
                           <<>>
                   end,
    DfltTag = otters_config:read(zipkin_add_host_tag_to_span, undefined),
    <<?T_STRUCT, Size:32,
      << << (span_to_struct(S, DfltTag, DfltSrv,
                            UndefSrvLog, UndefSrvTag))/binary >>
         || S <- Spans>>/binary>>.


decode_spans(Data) ->
    {{struct, StructList}, _Rest} = decode_implicit_list(Data),
    [struct_to_span(S) || S <- StructList].

span_to_struct(#span{
                  id = Id,
                  trace_id = TraceId,
                  name = Name,
                  parent_id = ParentId,
                  logs = Logs,
                  tags = TagsM,
                  timestamp = Timestamp,
                  duration = Duration
                 }, DfltTag, DfltSrv, UndefSrvLog, UndefSrvTag) ->
    Tags = maps:to_list(TagsM),
    FinalTags = case DfltTag of
                    {Key, Value} ->
                        [{Key, {Value, default}} | Tags];
                    _ ->
                        Tags
                end,
    LogSize = length(Logs),
    TagSize = length(FinalTags),
    NameBin = otters_lib:to_bin(Name),
    ParentBin =case ParentId of
                   undefined ->
                       <<>>;
                   ParentId ->
                       <<?T_I64, 5:16, ParentId:64/signed-integer>>
               end,
    LogsBin = << <<(log_to_annotation(Log, DfltSrv, UndefSrvLog))/binary>>
                 || Log <- Logs >>,
    TagsBin =
        << <<(tag_to_binary_annotation(Tag, DfltSrv, UndefSrvTag))/binary>>
           || Tag <- FinalTags >>,
    <<
      %% Header
      ?T_I64,    1:16, TraceId:64/signed-integer,
      ?T_STRING, 3:16, (byte_size(NameBin)):32, NameBin/binary,
      ?T_I64,    4:16, Id:64/signed-integer,
      ParentBin/binary,
      %% Logs
      ?T_LIST,   6:16, ?T_STRUCT, LogSize:32, LogsBin/bytes,
      %% Tags
      ?T_LIST,   8:16, ?T_STRUCT, TagSize:32, TagsBin/bytes,
      %% Tail
      ?T_I64,   10:16, Timestamp:64/signed-integer,
      ?T_I64,   11:16, Duration:64/signed-integer,
      0
    >>.

service_to_bin(undefined, _, _DfltSrv, UndefServ) ->
    UndefServ;
service_to_bin(default, _, DfltSrv, _UndefServ) ->
    DfltSrv;
service_to_bin(Service, ID, _DfltSrv, _UndefServ) ->
    HostBin = host_to_struct(Service),
    <<?T_STRUCT, ID:16, HostBin/binary>>.

log_to_annotation({Timestamp, Text}, DfltSrv, UndefSrv) ->
    log_to_annotation({Timestamp, Text, undefined}, DfltSrv, UndefSrv);

log_to_annotation({Timestamp, Text, Service}, DfltSrv, UndefSrv) ->
    TextBin = otters_lib:to_bin(Text),
    SrvBin = service_to_bin(Service, 3, DfltSrv, UndefSrv),
    <<?T_I64,    1:16, Timestamp:64/signed-integer,
      ?T_STRING, 2:16, (byte_size(TextBin)):32, TextBin/binary,
      SrvBin/binary,
      0>>.

tag_to_binary_annotation({Key, {Value, Service}}, DfltSrv, UndefServ)
  when is_binary(Key) ->
    ValueBin = otters_lib:to_bin(Value),
    SrvBin = service_to_bin(Service, 4, DfltSrv, UndefServ),
    <<?T_STRING, 1:16, (byte_size(Key)):32, Key/binary,
      ?T_STRING, 2:16, (byte_size(ValueBin)):32, ValueBin/binary,
      ?T_I32,    3:16, 6:32/signed-integer,
      SrvBin/binary,
      0>>;

tag_to_binary_annotation({Key, {Value, Service}}, DfltSrv, UndefServ) ->
    tag_to_binary_annotation({otters_lib:to_bin(Key), {Value, Service}},
                             DfltSrv, UndefServ).

host_to_struct(default) ->
    DefaultService = otters_config:read(
                       zipkin_tag_host_service,
                       atom_to_list(node())
                      ),
    host_to_struct(DefaultService);
host_to_struct(Service)
  when is_binary(Service);
       is_list(Service);
       is_atom(Service) ->
    host_to_struct({
                     otters_lib:to_bin(Service),
                     otters_config:read(zipkin_tag_host_ip, {127, 0, 0, 1}),
                     otters_config:read(zipkin_tag_host_port, 0)
                   });
host_to_struct({Service, Ip, Port}) ->
    IPInt = otters_lib:ip_to_i32(Ip),
    <<?T_I32,    1:16, IPInt:32/signed-integer,
      ?T_I16,    2:16, Port:16/signed-integer,
      ?T_STRING, 3:16, (byte_size(Service)):32, Service/binary,
      0>>.

struct_to_span(StructData) ->
    struct_to_span(StructData, #span{}).

struct_to_span([{1, i64, TraceId}| Rest], Span) ->
    struct_to_span(Rest, Span#span{trace_id = TraceId});
struct_to_span([{3, string, Name}| Rest], Span) ->
    struct_to_span(Rest, Span#span{name = Name});
struct_to_span([{4, i64, Id}| Rest], Span) ->
    struct_to_span(Rest, Span#span{id = Id});
struct_to_span([{5, i64, ParentId}| Rest], Span) ->
    struct_to_span(Rest, Span#span{parent_id = ParentId});
struct_to_span([{6, list, {struct, Annotations}}| Rest], Span) ->
    Logs = [annotation_to_log(Annotation) || Annotation <- Annotations],
    struct_to_span(Rest, Span#span{logs = Logs});
struct_to_span([{8, list, {struct, BinAnnotations}}| Rest], Span) ->
    Tags = [bin_annotation_to_tag(BinAnnotation) ||
               BinAnnotation <- BinAnnotations],
    struct_to_span(Rest, Span#span{tags = maps:from_list(Tags)});
struct_to_span([{10, i64, Timestamp}| Rest], Span) ->
    struct_to_span(Rest, Span#span{timestamp = Timestamp});
struct_to_span([{11, i64, Duration}| Rest], Span) ->
    struct_to_span(Rest, Span#span{duration = Duration});
struct_to_span([_ | Rest], Span) ->
    struct_to_span(Rest, Span);
struct_to_span([], Span) ->
    Span.

annotation_to_log(StructData) ->
    annotation_to_log(StructData, {undefined, undefined, undefined}).

annotation_to_log([{1, i64, Timestamp} | Rest], {_, Text, Host}) ->
    annotation_to_log(Rest, {Timestamp, Text, Host});
annotation_to_log([{2, string, Text} | Rest], {Timestamp, _, Host}) ->
    annotation_to_log(Rest, {Timestamp, Text, Host});
annotation_to_log([{3, struct, HostStruct} | Rest], {Timestamp, Text, _}) ->
    annotation_to_log(Rest, {Timestamp, Text, struct_to_host(HostStruct)});
annotation_to_log([_ | Rest], Log) ->
    annotation_to_log(Rest, Log);
annotation_to_log([], {Timestamp, Text, undefined}) ->
    {Timestamp, Text};
annotation_to_log([], Log) ->
    Log.

bin_annotation_to_tag(StructData) ->
    bin_annotation_to_tag(StructData, {<<"">>, {<<"">>, undefined}}).

-spec bin_annotation_to_tag(
        [term()], {binary(), {binary(), otters:service() | undefined}})
                           -> {binary(),
                               {binary(), otters:service() | undefined}}.
bin_annotation_to_tag([{1, string, Key} | Rest], {_, {Value, Host}}) ->
    bin_annotation_to_tag(Rest, {Key, {Value, Host}});
bin_annotation_to_tag([{2, string, Value} | Rest], {Key, {_, Host}}) ->
    bin_annotation_to_tag(Rest, {Key, {Value, Host}});
bin_annotation_to_tag([{4, struct, HostStruct} | Rest], {Key, {Value, _}}) ->
    bin_annotation_to_tag(Rest, {Key, {Value, struct_to_host(HostStruct)}});
bin_annotation_to_tag([_ | Rest], Tag) ->
    bin_annotation_to_tag(Rest, Tag);
bin_annotation_to_tag([], Tag) ->
    Tag.

struct_to_host(StructData) ->
    struct_to_host(StructData, {undefined, undefined, undefined}).

struct_to_host([{1, i32, IntIp} | Rest], {Service, _, Port}) ->
    <<Ip1, Ip2, Ip3, Ip4>> = <<IntIp:32>>,
    struct_to_host(Rest, {Service, {Ip1, Ip2, Ip3, Ip4}, Port});
struct_to_host([{2, i16, Port} | Rest], {Service, Ip, _}) ->
    struct_to_host(Rest, {Service, Ip, Port});
struct_to_host([{3, string, Service} | Rest], {_, Ip, Port}) ->
    struct_to_host(Rest, {Service, Ip, Port});
struct_to_host([_ | Rest], Host) ->
    struct_to_host(Rest, Host);
struct_to_host([], Host) ->
    Host.

%% encode/decode basic thrift binary data
%% e.g. The transport (e.g. HTTP) data is an "implicit" list starting
%% with the element type, and number of elements ..
decode_implicit_list(BinaryData) ->
    decode(list, BinaryData).

%% Decoding functions
decode(<<TypeId, Id:16, Data/bytes>>) ->
    Type = map_type(TypeId),
    {Val, Rest} = decode(Type, Data),
    {{Id, Type, Val}, Rest}.

decode(bool, <<Val, Rest/bytes>>) ->
    {Val == 1, Rest};
decode(byte, <<Val, Rest/bytes>>) ->
    {Val, Rest};
decode(double, <<Val:64/float, Rest/bytes>>) ->
    {Val, Rest};
decode(i16, <<Val:16/signed-integer, Rest/bytes>>) ->
    {Val, Rest};
decode(i32, <<Val:32/signed-integer, Rest/bytes>>) ->
    {Val, Rest};
decode(i64, <<Val:64/signed-integer, Rest/bytes>>) ->
    {Val, Rest};
decode(string, <<ByteLen:32, BytesAndRest/bytes>>) ->
    <<Bytes:ByteLen/bytes, Rest/bytes>> = BytesAndRest,
    {Bytes, Rest};
decode(struct, Data) ->
    decode_struct(Data, []);
decode(map, <<KeyTypeId, ValTypeId, Size:32, KVPsAndRest/bytes>>) ->
    decode_map(
      map_type(KeyTypeId),
      map_type(ValTypeId),
      Size,
      KVPsAndRest,
      []
     );
%% Lists and Sets are encoded the same way
decode(set, Data) ->
    decode(list, Data);
decode(list, <<ElementTypeId, Size:32, ElementsAndRest/bytes>>) ->
    decode_list(
      map_type(ElementTypeId),
      Size,
      ElementsAndRest,
      []
     ).

%% Helpers

decode_struct(Data, Acc) ->
    case decode(Data) of
        {Val, <<0, Rest/bytes>>} ->
            {lists:reverse([Val | Acc]), Rest};
        {Val, Rest} ->
            decode_struct(Rest, [Val | Acc])
    end.

decode_map(KeyType, ValType, 0, Rest, Acc) ->
    {{{KeyType, ValType}, lists:reverse(Acc)}, Rest};
decode_map(KeyType, ValType, Size, KVPsAndRest, Acc) ->
    {Key, ValAndRest} = decode(KeyType, KVPsAndRest),
    {Val, Rest} =  decode(ValType, ValAndRest),
    decode_map(KeyType, ValType, Size-1, Rest, [{Key, Val} | Acc]).

decode_list(ElementType, 0, Rest, Acc) ->
    {{ElementType, lists:reverse(Acc)}, Rest};
decode_list(ElementType, Size, Elements, Acc) ->
    {Data, Rest} = decode(ElementType, Elements),
    decode_list(ElementType, Size-1, Rest, [Data | Acc]).

map_type(2)     -> bool;
map_type(3)     -> byte;
map_type(4)     -> double;
map_type(?T_I16)    -> i16;
map_type(?T_I32)    -> i32;
map_type(?T_I64)    -> i64;
map_type(?T_STRING) -> string;
map_type(?T_STRUCT) -> struct;
map_type(13)    -> map;
map_type(14)    -> set;
map_type(?T_LIST)   -> list;
map_type(bool)  -> 2;
map_type(byte)  -> 3;
map_type(double)-> 4;
map_type(i16)   -> ?T_I16;
map_type(i32)   -> ?T_I32;
map_type(i64)   -> ?T_I64;
map_type(string)-> ?T_STRING;
map_type(struct)-> ?T_STRUCT;
map_type(map)   -> 13;
map_type(set)   -> 14;
map_type(list)  -> ?T_LIST.
