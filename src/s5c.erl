-module(s5c).

%% For library
-export([curl/3]).

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

%% For escript
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc s3curl --id foobar -put @file -- -s -v -x localhost:8080 http://s3.amazonaws.com/buckett/keyy -H 'x-maybe-foolish: true'
curl(MetaOpts, URL, CurlOpts) ->
    Req0 = s5c_http:new_request(URL, CurlOpts),
    Req1 = s5c_http:update_meta(Req0, MetaOpts),
    Res = s5c_http:exec(Req1),
    io:format("~p => ~p~n", [Req1, Res]),
    io:format("~s", [s5c_http:body(Res)]).

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

%% escript Entry point
main(Args) ->
    s5c_console:main(Args).
