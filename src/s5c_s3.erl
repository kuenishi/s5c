-module(s5c_s3).

-export([sign/5]).


%% @doc Version 2
%% sign(_Req = #request{verb=Verb, headers=Hdrs,
%%                      bucket=Bucket, key=Key},
%%     KeySecret) ->
sign(Verb, Hdrs, Bucket, Key, KeySecret) ->
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
    STS = [s5c_http:verb(Verb), "\n",
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


