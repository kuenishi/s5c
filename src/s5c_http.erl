-module(s5c_http).

-export([new_request/2, update_meta/2, exec/1]).

-record(request, {
          dst_addr :: inet:ip_address() | inet:hostname(),
          dst_port :: inet:port_number(),
          transport = http :: http | https,
          silent = false :: boolean(),
          visible = false :: boolean(),
          verb :: get|put|head|post|delete|options,
          resource :: iolist(),
          headers = [] :: proplists:proplist(),
          body :: {raw, binary()} | {file, filename:filename()} | {stream, term()}
         }).

-record(response, {
          code :: 100..599,
          headers :: proplists:proplist(),
          body :: {raw, binary()} | {file, filename:filename()} | {stream, term()}
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
new_request(URL, _CurlOpts) ->
    Date = httpd_util:rfc1123_date(),
    {Scheme, Host, Port, Path} = decode_url(URL),
    #request{
       dst_addr = Host, dst_port = Port,
       transport = Scheme, resource = Path,
       headers = [{date, Date}]
      }.

-spec update_meta(request(), proplists:proplist()) -> request().
update_meta(Req = #request{headers=Hdrs}, MetaOpts) ->
    ID = proplists:get_value(id, MetaOpts),
    Info = s5c_config:get(ID),
    KeyId = proplists:get_value(key_id, Info),
    KeySecret = proplists:get_value(key_secret, Info),
    SignedString = sign(Req, KeySecret),
    Req#request{headers=[{'Authorize', [KeyId, SignedString]}|Hdrs]}.

-spec exec(request()) -> {ok, response()} | {error, term()}.
exec(Req = #request{dst_addr=Addr, dst_port=Port}) ->
    Options = [http],
    {ok, Socket} = gen_tcp:connect(Addr, Port, Options),
    %% _Conn = #connection{socket=Socket, options=_Opt},
    ok = gen_tcp:send(Socket, req2hdr(Req)),
    {ok, Resp} = gen_tcp:recv(Socket, 0, 6000),
    ok = gen_tcp:close(Socket),
    io:format("Resp: ~p~n", [Resp]),
    #response{}.

%%====================================================================
%% Internal functions
%%====================================================================

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
    [ atom_to_list(Key), Value ];
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

    {HostPort, Path} = split(Rest1, $/, ""),
    {Host, Port0} = split(HostPort, $:, ""),
    Port = case Port0 of
               "" ->
                   case Scheme of
                       http -> 80;
                       https -> 443
                   end;
               _ -> Port0
           end,
    {Scheme, Host, Port, Path}.

split([Ch|Rest], Ch, Prefix) ->
    {lists:reverse(Prefix), Rest};
split([Ch|Rest], Ch0, Prefix) ->
    split(Rest, Ch0, [Ch|Prefix]).

%% @doc Version 2
sign(Req = #request{verb=Verb, headers=Hdrs, resource=Resource},
     KeySecret) ->                                     
    Headers = Hdrs,
    AmazonHeaders = riak_cs_wm_utils:extract_amazon_headers(Headers),
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
    STS = [atom_to_list(Verb), "\n",
           CMD5,
           "\n",
           ContentType,
           "\n",
           Date,
           AmazonHeaders,
           Resource],
    %% _ = lager:debug("STS: ~p", [STS]),

    base64:encode_to_string(crypto:hmac(sha, KeySecret, STS)).
    

    
