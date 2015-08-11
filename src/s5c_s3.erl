-module(s5c_s3).

-export([sign/5]).
%% http://docs.aws.amazon.com/AmazonS3/latest/API/APIRest.html
%% For library
-export([connect/3, disconnect/1,

         %% Bucket API
         put_bucket/2, get_bucket/2, delete_bucket/2,
         put_bucket_acl/2, get_bucket_acl/2,
         put_bucket_policy/2, get_bucket_policy/2, delete_bucket_policy/2,
         get_bucket_location/2,

         %% Object API
         put_object/3, get_object/3, delete_object/3, head_object/3, post_object/3,
         put_object_copy/3,
         initiate_multipart_upload/3, upload_part/2, upload_part_copy/2,
         complete_multipart_upload/3, abort_multipart_upload/3, list_parts/2

        ]).



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


connect(_, _, _) -> error. disconnect(_) -> error.

%% Bucket API
put_bucket(_, _) -> error.
get_bucket(_, _) -> error.
delete_bucket(_, _) -> error.
put_bucket_acl(_, _) -> error.
get_bucket_acl(_, _) -> error.
put_bucket_policy(_, _) -> error.
get_bucket_policy(_, _) -> error.
delete_bucket_policy(_, _) -> error.
get_bucket_location(_, _) -> error.

%% Object API
put_object(_, _, _) -> error.
get_object(_, _, _) -> error.
delete_object(_, _, _) -> error.
head_object(_, _, _) -> error.
post_object(_, _, _) -> error.
put_object_copy(_, _, _) -> error.
initiate_multipart_upload(_, _, _) -> error.
upload_part(_, _) -> error.
upload_part_copy(_, _) -> error.
complete_multipart_upload(_, _, _) -> error.
abort_multipart_upload(_, _, _) -> error.
list_parts(_, _) -> error.
