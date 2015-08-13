-module(s5c_s2).

-export([main/2,
         get_user/2, get_users/1,
         create_user/3,
         get_stats/1,
         get_usage/2, get_access/2]).

main(["get", "user", KeyId], Opts) -> get_user(KeyId, Opts);
main(["get", "users"], Opts) -> get_users(Opts);
main(["create", "user", Name, Address], Opts) -> create_user(Name, Address, Opts);
main(["get", "stats"], Opts) -> get_stats(Opts);
main(["get", "usage" | User], Opts) -> get_usage(User, Opts);
main(["get", "access"| User], Opts) -> get_access(User, Opts).

get_user(_, _) -> ok.
get_users(_Opts) ->
    URL = "http://riak-cs.s3.amazonaws.com/users",
    Req = s5c_http:new_request(URL, [{header, "accept: application/json"},
                                     {proxy, "localhost:8080"}]),
    Req1 = s5c_s3:sign(Req, local),
    {ok, Conn} = s5c_http:send(Req1),
    {ok, Res} = s5c_http:recv(Conn),
    {chunked, Chunks} = s5c_http:body(Res),
    pp_header(Chunks),
    [begin
         case proplists:get_value('Content-Type', Hdrs) of
             <<"application/json">> ->
                 lists:foreach(fun pp_user/1,
                               jsone:decode(Body));
             _ ->
                 pass
         end
     end || {Hdrs, Body} <- Chunks].

create_user(_, _, _) -> ok.
get_stats(_) ->
    URL = "http://riak-cs.s3.amazonaws.com/stats",
    Req = s5c_http:new_request(URL, [{header, "accept: application/json"},
                                     {proxy, "localhost:8080"}]),
    Req1 = s5c_s3:sign(Req, local),
    {ok, Conn} = s5c_http:send(Req1),
    {ok, Res} = s5c_http:recv(Conn),
    {raw, Json} = s5c_http:body(Res),
    io:format("~p", [jsone:decode(Json)]).

get_usage("", _Opts) ->
    error;
get_usage(User, _Opts) when is_list(User) ->
    Start = "20150801T000000Z",
    %%Start = proplists:get(
    End = "20150901T000000Z",
    Path = filename:join(["/", User, "bj", Start, End]),
    URL = "http://riak-cs.s3.amazonaws.com/usage/" ++ Path,
    Req = s5c_http:new_request(URL, [{header, "accept: application/json"},
                                     {proxy, "localhost:8080"}]),
    Req1 = s5c_s3:sign(Req, local),
    {ok, Conn} = s5c_http:send(Req1),
    {ok, Res} = s5c_http:recv(Conn),
    io:format("~p", [s5c_http:body(Res)]).

get_access(_, _) -> ok.


%% Internal Functions

pp_header([]) -> pass;
pp_header(_Chunks) ->
    io:format("~-20s\t~-10s\tname~n", [key_id, key_secret]).

-spec pp_user({proplists:proplist()}) -> no_return().
pp_user({User}) ->
    %% io:format("~p~n", [User]),
    KeyId = proplists:get_value(<<"key_id">>, User),
    Name = proplists:get_value(<<"name">>, User),
    KeySecret = proplists:get_value(<<"key_secret">>, User),
    io:format("~-20s\t~-5s...\t~s~n", [KeyId, KeySecret, Name]).
