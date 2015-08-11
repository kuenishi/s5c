-module(s5c).

%% For library
-export([curl/3]).

%% For escript
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc s3curl --id foobar -put @file -- -s -v -x localhost:8080 http://s3.amazonaws.com/buckett/keyy -H 'x-maybe-foolish: true'
curl(MetaOpts, URL, CurlOpts) ->
    Req0 = s5c_http:new_request(URL, CurlOpts),
    Req1 = s5c_http:update_meta(Req0, MetaOpts),
    {ok, Conn} = s5c_http:send(Req1),
    {ok, Res} = s5c_http:recv(Conn),
    %% io:format("~p => ~p~n", [Req1, Res]),
    {ok, s5c_http:body(Res)}.

%% escript Entry point
main(Args) ->
    s5c_console:main(Args).
