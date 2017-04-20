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

-module(otters_zipkin_encoder).
-export([encode/1]).
-include("otters.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-define(T_I16,     6).
-define(T_I32,     8).
-define(T_I64,    10).
-define(T_STRING, 11).
-define(T_STRUCT, 12).
-define(T_LIST,   15).


-record(conf,
        {
          service              :: binary(),
          ip                   :: non_neg_integer(),
          port                 :: non_neg_integer(),

          add_tag_to_log       :: boolean(),
          add_tag_to_tag       :: boolean(),
          add_tag              :: undefined | {otters:info(), otters:info()},
          server_bin_log_dflt  = <<>> :: binary(),
          server_bin_tag_dflt  = <<>> :: binary(),
          server_bin_log_undef = <<>> :: binary(),
          server_bin_tag_undef = <<>> :: binary()
        }).

encode(#span{} = S) ->
    encode([S]);

encode(Spans) when is_list(Spans) ->
    Size = length(Spans),
    Cfg = defaults(),
    <<?T_STRUCT, Size:32,
      << << (encode_span(S, Cfg))/binary >>
         || S <- Spans>>/binary>>.

defaults() ->
    DefaultService = cfg(zipkin_tag_host_service, atom_to_binary(node(), utf8)),
    DfltIP = otters_lib:ip_to_i32(cfg(zipkin_tag_host_ip, {127, 0, 0, 1})),
    DfltPort = cfg(zipkin_tag_host_port, 0),

    AddDfltToLog = cfg(zipkin_add_default_service_to_logs, false),
    AddDfltToTag = cfg(zipkin_add_default_service_to_tags, false),
    C0 =     #conf{
                service = DefaultService,
                ip      = DfltIP,
                port    = DfltPort,

                add_tag_to_log = AddDfltToLog,
                add_tag_to_tag = AddDfltToTag,
                add_tag        = cfg(zipkin_add_host_tag_to_span, undefined)
               },

    HostBin = encode_host(default, C0),
    LogHostBin = <<?T_STRUCT, 3:16, HostBin/binary>>,
    TagHostBin = <<?T_STRUCT, 4:16, HostBin/binary>>,
    C0#conf{

      server_bin_log_dflt = LogHostBin,
      server_bin_tag_dflt = TagHostBin,

      server_bin_log_undef = case AddDfltToLog of
                                 true ->
                                     LogHostBin;
                                 _ ->
                                     <<>>
                             end,
      server_bin_tag_undef = case AddDfltToTag of
                                 true ->
                                     TagHostBin;
                                 _ ->
                                     <<>>
                             end}.

encode_span(#span{
               id = Id,
               trace_id = TraceId,
               name = Name,
               parent_id = ParentId,
               logs = Logs,
               tags = Tags,
               timestamp = Timestamp,
               duration = Duration
              }, Cfg) ->
    {Tags0Bin, TagSize}
        = case Cfg#conf.add_tag of
              {Key, Value} ->
                  {encode_tag({otters_lib:to_bin(Key), {Value, default}}, Cfg),
                  maps:size(Tags) + 1};
              _ ->
                  {<<>>, maps:size(Tags)}
          end,
    LogSize = length(Logs),
    NameBin = otters_lib:to_bin(Name),
    ParentBin = case ParentId of
                    undefined ->
                        <<>>;
                    ParentId ->
                        <<?T_I64, 5:16, ParentId:64/signed-integer>>
                end,
    <<
      %% Header
      ?T_I64,    1:16, TraceId:64/signed-integer,
      ?T_STRING, 3:16, (byte_size(NameBin)):32, NameBin/binary,
      ?T_I64,    4:16, Id:64/signed-integer,
      ParentBin/binary,
      %% Logs
      ?T_LIST,   6:16, ?T_STRUCT, LogSize:32,
      (<< <<(encode_log(Log, Cfg))/binary>> || Log <- Logs >>)/bytes,
      %% Tags
      ?T_LIST,   8:16, ?T_STRUCT, TagSize:32, Tags0Bin/binary,
      (<< <<(encode_tag(Tag, Cfg))/binary>>
          || Tag <- maps:to_list(Tags)>>)/bytes,
      %% Tail
      ?T_I64,   10:16, Timestamp:64/signed-integer,
      ?T_I64,   11:16, Duration:64/signed-integer,
      0>>.


encode_host(default, Cfg) ->
    encode_host(Cfg#conf.service, Cfg);

encode_host(Service, Cfg)
  when is_binary(Service) ->
    encode_host({Service, Cfg#conf.ip, Cfg#conf.port}, Cfg);

encode_host(Service, Cfg)
  when is_list(Service);
       is_atom(Service) ->
    encode_host({
                  otters_lib:to_bin(Service),
                  Cfg#conf.ip,
                  Cfg#conf.port
                }, Cfg);
encode_host({Service, Ip, Port}, _Cfg) when is_integer(Ip) ->
    <<?T_I32,    1:16, Ip:32/signed-integer,
      ?T_I16,    2:16, Port:16/signed-integer,
      ?T_STRING, 3:16, (byte_size(Service)):32, Service/binary,
      0>>;

encode_host({Service, Ip, Port}, _Cfg) ->
    IPInt = otters_lib:ip_to_i32(Ip),
    <<?T_I32,    1:16, IPInt:32/signed-integer,
      ?T_I16,    2:16, Port:16/signed-integer,
      ?T_STRING, 3:16, (byte_size(Service)):32, Service/binary,
      0>>.


%% logs w/ undefined service

%% binary text
encode_log({Timestamp, Text, undefined}, Cfg) when is_binary(Text) ->
    <<?T_I64,    1:16, Timestamp:64/signed-integer,
      ?T_STRING, 2:16, (byte_size(Text)):32, Text/binary,
      (Cfg#conf.server_bin_log_undef)/binary,
      0>>;

encode_log({Timestamp, Text, undefined}, Cfg) ->
    TextBin = otters_lib:to_bin(Text),
    <<?T_I64,    1:16, Timestamp:64/signed-integer,
      ?T_STRING, 2:16, (byte_size(TextBin)):32, TextBin/binary,
      (Cfg#conf.server_bin_log_undef)/binary,
      0>>;

%% logs w/ undefined service

%% binary text
encode_log({Timestamp, Text, default}, Cfg) when is_binary(Text) ->
    <<?T_I64,    1:16, Timestamp:64/signed-integer,
      ?T_STRING, 2:16, (byte_size(Text)):32, Text/binary,
      (Cfg#conf.server_bin_log_dflt)/binary,
      0>>;

encode_log({Timestamp, Text, default}, Cfg) ->
    TextBin = otters_lib:to_bin(Text),
    <<?T_I64,    1:16, Timestamp:64/signed-integer,
      ?T_STRING, 2:16, (byte_size(TextBin)):32, TextBin/binary,
      (Cfg#conf.server_bin_log_dflt)/binary,
      0>>;

%% Other services

encode_log({Timestamp, Text, Service}, Cfg) when is_binary(Text) ->
    SrvBin = encode_service(Service, log, Cfg),
    <<?T_I64,    1:16, Timestamp:64/signed-integer,
      ?T_STRING, 2:16, (byte_size(Text)):32, Text/binary,
      SrvBin/binary,
      0>>;

encode_log({Timestamp, Text, Service}, Cfg) ->
    TextBin = otters_lib:to_bin(Text),
    SrvBin = encode_service(Service, log, Cfg),
    <<?T_I64,    1:16, Timestamp:64/signed-integer,
      ?T_STRING, 2:16, (byte_size(TextBin)):32, TextBin/binary,
      SrvBin/binary,
      0>>.

%% Tags undefined
encode_tag({Key, {Value, undefined}}, Cfg)
  when is_binary(Key), is_binary(Value) ->
    <<?T_STRING, 1:16, (byte_size(Key)):32, Key/binary,
      ?T_STRING, 2:16, (byte_size(Value)):32, Value/binary,
      ?T_I32,    3:16, 6:32/signed-integer,
      (Cfg#conf.server_bin_tag_undef)/binary,
      0>>;

encode_tag({Key, {Value, undefined}}, Cfg)
  when is_binary(Key) ->
    ValueBin = otters_lib:to_bin(Value),
    <<?T_STRING, 1:16, (byte_size(Key)):32, Key/binary,
      ?T_STRING, 2:16, (byte_size(ValueBin)):32, ValueBin/binary,
      ?T_I32,    3:16, 6:32/signed-integer,
      (Cfg#conf.server_bin_tag_undef)/binary,
      0>>;

%% TAgs default
encode_tag({Key, {Value, default}}, Cfg)
  when is_binary(Key), is_binary(Value) ->
    <<?T_STRING, 1:16, (byte_size(Key)):32, Key/binary,
      ?T_STRING, 2:16, (byte_size(Value)):32, Value/binary,
      ?T_I32,    3:16, 6:32/signed-integer,
      (Cfg#conf.server_bin_tag_dflt)/binary,
      0>>;

encode_tag({Key, {Value, default}}, Cfg)
  when is_binary(Key) ->
    ValueBin = otters_lib:to_bin(Value),
    <<?T_STRING, 1:16, (byte_size(Key)):32, Key/binary,
      ?T_STRING, 2:16, (byte_size(ValueBin)):32, ValueBin/binary,
      ?T_I32,    3:16, 6:32/signed-integer,
      (Cfg#conf.server_bin_tag_dflt)/binary,
      0>>;

%% Other services
encode_tag({Key, {Value, Service}}, Cfg)
  when is_binary(Key), is_binary(Value) ->
    SrvBin = encode_service(Service, tag, Cfg),
    <<?T_STRING, 1:16, (byte_size(Key)):32, Key/binary,
      ?T_STRING, 2:16, (byte_size(Value)):32, Value/binary,
      ?T_I32,    3:16, 6:32/signed-integer,
      SrvBin/binary,
      0>>;
encode_tag({Key, {Value, Service}}, Cfg)
  when is_binary(Key) ->
    ValueBin = otters_lib:to_bin(Value),
    SrvBin = encode_service(Service, tag, Cfg),
    <<?T_STRING, 1:16, (byte_size(Key)):32, Key/binary,
      ?T_STRING, 2:16, (byte_size(ValueBin)):32, ValueBin/binary,
      ?T_I32,    3:16, 6:32/signed-integer,
      SrvBin/binary,
      0>>.

encode_service(Service, log, Cfg) ->
    HostBin = encode_host(Service, Cfg),
    <<?T_STRUCT, 3:16, HostBin/binary>>;
encode_service(Service, tag, Cfg) ->
    HostBin = encode_host(Service, Cfg),
    <<?T_STRUCT, 4:16, HostBin/binary>>.


cfg(K, D) ->
    otters_config:read(K, D).
