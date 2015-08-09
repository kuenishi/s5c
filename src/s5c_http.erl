-module(s5c_http).

-export([new_request/2, update_meta/2, exec/1, body/1]).

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
          socket :: inet:socket(),
          options = [] :: proplists:proplist()
         }).

-type request() :: #request{}.
-type response() :: #response{}.
-type connection() :: #connection{}.
-export_type([request/0, response/0, connection/0]).

-spec new_request(string(), proplists:proplist()) -> request().
new_request(URL, CurlOpts) ->
    Date = httpd_util:rfc1123_date(),
    {Scheme, Host, Port0, Bucket, Path} = decode_url(URL),
    io:format("~p", [CurlOpts]),
    {ProxyHost, ProxyPort} = case proplists:get_value(proxy, CurlOpts) of
                       undefined -> {Host, Port0};
                       HostPort -> host_port(HostPort, Port0)
                   end,
    #request{
       dst_addr = ProxyHost, dst_port = ProxyPort,
       transport = Scheme, resource = Path,
       bucket = Bucket, key = Path,
       headers = [{date, Date}, {host, Host}]
      }.

-spec update_meta(request(), proplists:proplist()) -> request().
update_meta(Req = #request{headers=Hdrs}, MetaOpts) ->
    %% Authrize header
    ID = proplists:get_value(id, MetaOpts),
    {KeyId, KeySecret} = s5c_config:get(ID),
    SignedString = sign(Req, KeySecret),
    Req#request{headers=[{'Authorization',
                          ["AWS ", KeyId, $:, SignedString]}|Hdrs]}.

-spec exec(request()) -> {ok, response()} | {error, term()}.
exec(Req = #request{dst_addr=Addr, dst_port=Port}) ->
    Options = [binary, {active, false}],
    {ok, Socket} = gen_tcp:connect(Addr, Port, Options),
    %% _Conn = #connection{socket=Socket, options=_Opt},
    %% io:format("~s:~p <= ~s~n", [Addr, Port, req2hdr(Req)]),
    ok = gen_tcp:send(Socket, req2hdr(Req)),
    {all_header, Resp} = build_response(Socket, #response{}),
    Resp1 = resume_all(Socket, Resp),
    ok = gen_tcp:close(Socket),
    Resp1.

body(#response{body={raw,Body}}) ->
    Body.

%%====================================================================
%% Internal functions
%%====================================================================

resume_all(Socket, #response{headers=Hdrs, body={raw, Body}} = Resp) ->
    case proplists:get_value("Content-Length", Hdrs) of
        undefined ->
            ok;
        LenStr ->
            case list_to_integer(LenStr) of
                Len when Len =< byte_size(Body) ->
                    io:format("here>> ~p", [Len]),
                    Resp;
                _Len ->
                    {ok, Bin} = gen_tcp:recv(Socket, 0),
                    NewBody = <<Body/binary, Bin/binary>>,
                    io:format("here>> ~p", [NewBody]),
                    resume_all(Socket, Resp#response{body={raw, NewBody}})
            end
    end.

build_response(Socket, Res0) ->
    {ok, Data} = gen_tcp:recv(Socket, 0),
    io:format("Resp: ~p~n", [Data]),
    case parse_http_response(Data, Res0) of
        {partial_header, Res} ->
            build_response(Socket, Res);
        Other ->
            Other
    end.

parse_http_response(Bin, Res = #response{headers=Hdrs0}) ->
    case binary:split(Bin, <<"\r\n">>, []) of
        [<<>>, Rest] ->
            {all_header, Res#response{body={raw, Rest}}};
        [<<"HTTP/", RestBin/binary>>, Rest] ->
            %% Parse first line here
            [Vsn, CodeMsg] = binary:split(RestBin, <<" ">>, []),
            [Code, Msg] = binary:split(CodeMsg, <<" ">>, []),
            Res1 = Res#response{code = list_to_integer(binary_to_list(Code)),
                                version = Vsn,
                                status = Msg},
            parse_http_response(Rest, Res1);
        [HeaderLine, Rest] ->
            {K, V} = split(binary_to_list(HeaderLine), $:, ""),
            Hdrs = [{string:strip(K),
                     string:strip(V)}|Hdrs0],
            parse_http_response(Rest,
                                Res#response{headers=Hdrs});
        [_PartialHeader] ->
            {partial_header, Res}
    end.

%% http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
req2hdr(#request{verb=Verb, resource=Path, headers=Hdrs}) ->
    [verb(Verb), 32, Path, 32, <<"HTTP/1.1">>, $\r, $\n,
     [ hdr2iolist(Hdr) || Hdr <- Hdrs ],
     $\r, $\n].

verb(put) -> <<"PUT">>;
verb(get) -> <<"GET">>;
verb(options) -> <<"OPTIONS">>;
verb(post) -> <<"POST">>;
verb(delete) -> <<"DELETE">>;
verb(head) -> <<"HEAD">>.

hdr2iolist({Key, Value}) when is_atom(Key) ->
    [ atom_to_list(Key), $:, " ", Value, $\r, $\n];
hdr2iolist({Key, Value}) ->
    [ Key, Value ].

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

%% @doc Version 2
sign(_Req = #request{verb=Verb, headers=Hdrs,
                     bucket=Bucket, key=Key},
     KeySecret) ->
    Headers = normalize(Hdrs),
    AmazonHeaders = extract_amazon_headers(Headers),
    Date = case proplists:get_value("expires", Headers) of
               undefined ->
                   case proplists:is_defined("x-amz-date", Headers) of
                       true ->  "\n";
                       false -> [proplists:get_value(date, Headers), "\n"]
                   end;
               Expires ->
                   Expires ++ "\n"
           end,
    CMD5 = case proplists:get_value("content-md5", Headers) of
               undefined -> [];
               CMD5_0 ->    CMD5_0
           end,
    ContentType = case proplists:get_value("content-type", Headers) of
                      undefined -> [];
                      ContentType0 -> ContentType0
                  end,
    Resource = [$/, Bucket, Key],
    STS = [verb(Verb), "\n",
           CMD5,
           "\n",
           ContentType,
           "\n",
           Date,
           AmazonHeaders,
           Resource],
    %% _ = lager:debug("STS: ~p", [STS]),
    io:format("~s~n", [STS]),
    base64:encode_to_string(crypto:hmac(sha, KeySecret, STS)).

normalize(Hdrs) ->
    lists:map(fun({K, V}) when is_list(K) -> {string:to_lower(K), V};
                 (Pair) -> Pair
              end, Hdrs).

extract_amazon_headers(Hdrs) ->
    lists:filter(fun({"x-amz-" ++ _, _}) -> true;
                    (_) -> false end, Hdrs).
