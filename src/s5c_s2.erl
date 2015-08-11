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
    Req1 = s5c_http:update_meta(Req, [{id, local}]),
    {ok, Conn} = s5c_http:send(Req1),
    {ok, Res} = s5c_http:recv(Conn),
    io:format("~p", [s5c_http:body(Res)]).

create_user(_, _, _) -> ok.
get_stats(_) -> ok.
get_usage(_, _) -> ok. get_access(_, _) -> ok.
