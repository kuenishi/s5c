-module(s5c_http).

-export([connect/3, connect/1, disconnect/1,
         new_request/2,
         send/2, recv/1]).

-export([verb/1, headers/1, bucket/1, key/1,
         body/1, add_header/2]).

-record(request, {
          dst_addr :: inet:ip_address() | inet:hostname(),
          dst_port :: inet:port_number(),
          transport = http :: http | https,
          silent = false :: boolean(),
          visible = false :: boolean(),
          verb = get :: get|put|head|post|delete|options,
          bucket,
          key,
          resource = "/" :: iolist(),
          headers = [] :: proplists:proplist(),
          body :: {raw, binary()} | {file, filename:filename()} | {stream, term()}
         }).

-record(response, {
          code :: 100..599,
          version :: binary(),
          status :: binary(),
          headers = [] :: proplists:proplist(),
          body :: {raw, binary()} | {stream, term()}
         }).

-record(connection, {
          ssl = false :: boolean(),
          socket :: inet:socket(),
          options = [] :: proplists:proplist()
         }).

-type request() :: #request{}.
-type response() :: #response{}.
-type connection() :: #connection{}.
-export_type([request/0, response/0, connection/0]).

-spec connect(inet:ip_address() | inet:hostname(),
              inet:port_number(),
              proplist:proplist()) ->
                     {ok, connection()} | {error, term()}.
connect(Host, Port, Opts) ->
    undefined = proplists:get_value(transport, Opts),
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Socket} ->
            io:format("connecting ~s:~p~n", [Host, Port]),
            {ok, #connection{socket=Socket, options=Opts}};
        Error ->
            Error
    end.

-spec connect(request()) -> {ok, connection()}
                                | {error, term()}.
connect(_Req = #request{dst_addr=Addr, dst_port=Port}) ->
    Options = [binary, {active, false}],
    connect(Addr, Port, Options).

disconnect(_Conn = #connection{socket=Socket, ssl=false}) ->
    ok = gen_tcp:close(Socket).    

-spec new_request(string(), proplists:proplist()) -> request().
new_request(URL, CurlOpts) ->
    Date = httpd_util:rfc1123_date(),
    {Scheme, Host, Port0, Bucket, Path} = decode_url(URL),
    io:format("~p", [CurlOpts]),
    {ProxyHost, ProxyPort} = case proplists:get_value(proxy, CurlOpts) of
                                 undefined -> {Host, Port0};
                                 HostPort -> host_port(HostPort, Port0)
                             end,
    Header0 = case proplists:get_value(header, CurlOpts) of
                  undefined -> [];
                  Header ->    [split(Header, $:, "")]
              end,
    #request{
       dst_addr = ProxyHost, dst_port = ProxyPort,
       transport = Scheme, resource = Path,
       bucket = Bucket, key = Path, %% TODO: move up to s5n_s3
       headers = [{date, Date}, {host, Host}] ++ Header0
      }.

-spec send(connection(), request()) -> ok | {error, term()}.
send(#connection{socket=Socket} = _Conn, Req) ->
    gen_tcp:send(Socket, req2hdr(Req)).

-spec recv(connection()) -> {ok, response()} | {error, term()}.
recv(#connection{socket=Socket} = _Conn) ->
    {ok, Resp} = build_response(Socket, #response{}),
    resume_all(Socket, Resp).

verb(#request{verb=Verb}) ->  verb2bin(Verb).
headers(#request{headers=Hdrs}) -> Hdrs.
bucket(#request{bucket=Bucket}) -> Bucket.
key(#request{key=Key}) -> Key.

body(#response{body=Body}) ->  Body.

add_header(Req = #request{headers=Hdrs}, {_, _} = Tuple) ->
    Req#request{headers=[Tuple|Hdrs]}.


%%====================================================================
%% Internal functions
%%====================================================================


resume_all(Socket, #response{headers=Hdrs, body={raw, Body}} = Resp) ->
    case proplists:get_value('Content-Length', Hdrs) of
        undefined ->
            case proplists:get_value('Transfer-Encoding', Hdrs) of
                <<"chunked">> ->
                    Marker = proplists:get_value('Content-Type', Hdrs),
                    <<"multipart/mixed; boundary=", Termial/binary>> = Marker,
                    resume_chunks(Socket, Resp#response{body={chunked,[]}},
                                  Body, <<"--", Termial/binary>>)
            end;
        LenStr ->
            case list_to_integer(binary_to_list(LenStr)) of
                Len when Len =< byte_size(Body) ->
                    io:format("here>> ~p", [Len]),
                    {ok, Resp};
                _Len ->
                    {ok, Bin} = gen_tcp:recv(Socket, 0),
                    NewBody = <<Body/binary, Bin/binary>>,
                    io:format("here>> ~p", [NewBody]),
                    resume_all(Socket, Resp#response{body={raw, NewBody}})
            end
    end.

resume_chunks(Socket, Resp, Data, Terminal) ->
    %% Marker = proplists:get_value("Content-Type", Hdrs),
    %% "multipart/mixed; " ++ Termial = Marker,
    %% io:format("Multipart ...until ~p~n", [Terminal]),
    {ok, Bin} = gen_tcp:recv(Socket, 0),
    NewBody = <<Data/binary, Bin/binary>>,
    handle_chunks(NewBody,
                  Socket,
                  Resp,
                  Terminal).

handle_chunks(Body, Socket, Resp = #response{body={chunked, Chunks}}, Terminal) ->
    case binary:split(Body, Terminal, []) of
        [<<$\r, $\n, Chunk/binary>>, L] ->
            {ok, {_, _Bin} = C} = decode_chunk(Chunk, []),
            %% io:format("Chunk. (~p)~n", [Bin]),
            NewChunks = [C|Chunks],
            handle_chunks(L, Socket, Resp#response{body={chunked,NewChunks}}, Terminal);

        [<<"--", _Chunk/binary>> | _] ->
            
            {ok, Resp#response{body={chunked, lists:reverse(Chunks)}}};

        [_Other, L] ->
            %% io:format("Other. ~p~n", [Other]),
            handle_chunks(L, Socket, Resp, Terminal);

        [Body] ->
            resume_chunks(Socket, Resp, Body, Terminal);

        [] ->
            resume_chunks(Socket, Resp, <<>>, Terminal)
    end.

decode_chunk(Chunk, Hdrs) ->
    case erlang:decode_packet(httph_bin, Chunk, []) of
        {ok, Packet, Rest} ->
            case Packet of
                {http_header, _, Key, _, Value} ->
                    decode_chunk(Rest, [{Key,Value}|Hdrs]);
                http_eoh ->
                    {ok, {Hdrs, Rest}};
                Error ->
                    Error
            end;
        E ->
            E
    end.

build_response(Socket, Res0) ->
    {ok, Data} = gen_tcp:recv(Socket, 0),
    {ok, FirstPacket, Rest} = erlang:decode_packet(http_bin, Data, []),
    {http_response, Vsn, Code, Status} = FirstPacket,
    Res = Res0#response{code=Code,
                        version=Vsn,
                        status=Status},
    build_response(Socket, Rest, Res).

build_response(Socket, Data, Res0 = #response{headers=Hdrs}) ->
    case erlang:decode_packet(httph_bin, Data, []) of
        {ok, Packet, Rest} ->
            case Packet of
                {http_header, _, K, _, V} ->
                    build_response(Socket, Rest,
                                   Res0#response{headers=[{K,V}|Hdrs]});
                http_eoh ->
                    {ok, Res0#response{body={raw, Rest}}}
            end;
        {more, Length} ->
            {ok, More} = gen_tcp:recv(Socket, Length),
            MoarData = << Data/binary, More/binary >>,
            build_response(Socket, MoarData, Res0);
        Error ->
            Error
    end.
                                  
    %% case parse_http_response(Data, Res0) of
    %%     {partial_header, Res} ->
    %%         build_response(Socket, Res);
    %%     Other ->
    %%         Other
    %% end.

%% parse_http_response(Bin, Res = #response{headers=Hdrs0}) ->
%%     case binary:split(Bin, <<"\r\n">>, []) of
%%         [<<>>, Rest] ->
%%             {all_header, Res#response{body={raw, Rest}}};
%%         [<<"HTTP/", RestBin/binary>>, Rest] ->
%%             %% Parse first line here
%%             [Vsn, CodeMsg] = binary:split(RestBin, <<" ">>, []),
%%             [Code, Msg] = binary:split(CodeMsg, <<" ">>, []),
%%             Res1 = Res#response{code = list_to_integer(binary_to_list(Code)),
%%                                 version = Vsn,
%%                                 status = Msg},
%%             parse_http_response(Rest, Res1);
%%         [HeaderLine, Rest] ->
%%             {K, V} = split(binary_to_list(HeaderLine), $:, ""),
%%             Hdrs = [{string:strip(K),
%%                      string:strip(V)}|Hdrs0],
%%             parse_http_response(Rest,
%%                                 Res#response{headers=Hdrs});
%%         [_PartialHeader] ->
%%             {partial_header, Res}
%%     end.

%% http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
req2hdr(#request{verb=Verb, resource=Path, headers=Hdrs}) ->
    [verb2bin(Verb), 32, Path, 32, <<"HTTP/1.1">>, $\r, $\n,
     [ hdr2iolist(Hdr) || Hdr <- Hdrs ],
     $\r, $\n].

verb2bin(put) -> <<"PUT">>;
verb2bin(get) -> <<"GET">>;
verb2bin(options) -> <<"OPTIONS">>;
verb2bin(post) -> <<"POST">>;
verb2bin(delete) -> <<"DELETE">>;
verb2bin(head) -> <<"HEAD">>.

hdr2iolist({Key, Value}) when is_atom(Key) ->
    [ atom_to_list(Key), $:, " ", Value, $\r, $\n];
hdr2iolist({Key, Value}) ->
    [ Key, $:, " ", Value, $\r, $\n ].

decode_url(URL) when is_binary(URL) ->
    decode_url(binary_to_list(URL));
decode_url(URL) when is_list(URL) ->
    {Scheme, Rest1} =
        case URL of
            "http://" ++ Rest0 ->
                {http, Rest0};
            "https://" ++ Rest0 ->
                {https, Rest0}
        end,

    {HostPort, Path0} = split(Rest1, $/, ""),
    {Host, Port} = host_port(HostPort,
                             case Scheme of
                                 http -> 80;
                                 https -> 443
                             end),
    {Bucket, _} = split(Host, $., ""),
    io:format(Bucket),
    Path = filename:join(["/", Path0]),
    {Scheme, Host, Port, Bucket, Path}.

host_port(HostPort, DefaultPort) ->
    {Host, Port0} = split(HostPort, $:, ""),
    Port = case Port0 of
               "" -> DefaultPort;
               _ -> list_to_integer(Port0)
           end,
    {Host, Port}.

split([], _, Prefix) ->
    {lists:reverse(Prefix), []};
split([Ch|Rest], Ch, Prefix) ->
    {lists:reverse(Prefix), Rest};
split([Ch|Rest], Ch0, Prefix) ->
    split(Rest, Ch0, [Ch|Prefix]).

