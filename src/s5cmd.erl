-module(s5cmd).

-export([main/2]).

main(["ls"], _Opts) ->
    {ok, Conn} = s5c_http:connect(foo),
    try
        Res = s5c_s3:get_service(Conn),
        io:format("~p~n", [Res])
    catch _:_ ->
            exit(boom)
    after
        s5c_http:disconnect(Conn)
    end;
    
main(["ls", Bucket], _Opts) ->
    {ok, Conn} = s5c_http:connect(foo),
    Prefix = <<>>,
    Delimiter = <<"/">>,
    StartKey = <<>>,
    try
        Res = s5c_s3:get_bucket(Conn, list_to_binary(Bucket),
                                StartKey, Delimiter, Prefix),
        io:format("~p~n", [Res])
    catch _:_ ->
            exit(boom)
    after
        s5c_http:disconnect(Conn)
    end.
