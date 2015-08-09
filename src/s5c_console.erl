-module(s5c_console).

%% For escript
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["generate"]) ->
    s5c_config:generate();

main(["curl"]) ->
    %% If usage/3 is used and written to standard_error in MacOS /
    %% 18.0 you'll find some poor OTP bug.
    getopt:usage(s3curl_option_spec() ++ curl_option_spec(),
                 "s5c curl",
                 standard_io);

main(["curl"|Args]) ->
    {S3CurlArgs, CurlArgs} = split_opts(Args, []),
    {ok, {S3CurlOpts, []}} = getopt:parse(s3curl_option_spec(), S3CurlArgs),
    {ok, {CurlOpts, [URL]}} = getopt:parse(curl_option_spec(), CurlArgs),
    Res = s5c:curl(S3CurlOpts, URL, CurlOpts),
    io:format("~p~n", [Res]);

main(["cmd", Args]) ->
    s5cmd:main(Args);

main(["cmd"|_]) ->
    getopt:usage(cmd_option_spec(), "s5c cmd", standard_io);

main(_) ->
    %% Arg0 = [ [["1234567890" || _ <- [1,2,3,4,5,6,7,8,9,0]], $\n]
    %%         || _ <- lists:seq(1, 20) ],
    %% Arg = iolist_to_binary(Arg0),
    %% io:format("~p, ~p", [whereis(standard_error), process_info(whereis(standard_error))]),
    %% R = io:put_chars(standard_error, Arg),
    %% io:format("~p, ~p", [R, whereis(standard_error)]),
    io:format(standard_error, "s5c [cmd [ARGS]|curl [ARGS]|generate]~n", []).


%%====================================================================
%% Internal functions
%%====================================================================

split_opts(["--"|CurlArgs], S3CurlArgs) ->
    {lists:reverse(S3CurlArgs), CurlArgs};
split_opts([Arg|CurlArgs], S3CurlArgs) ->
    split_opts(CurlArgs, [Arg|S3CurlArgs]).

%% my $usage = <<USAGE;
%% Usage $0 --id friendly-name (or AWSAccessKeyId) [options] -- [curl-options] [URL]
%%  options:
%%   --key SecretAccessKey       id/key are AWSAcessKeyId and Secret (unsafe)
%%   --contentType text/plain    set content-type header
%%   --acl public-read           use a 'canned' ACL (x-amz-acl header)
%%   --contentMd5 content_md5    add Content-MD5 header
%%   --calculateContentMd5       calculate Content-MD5 and add it
%%   --put <filename>            PUT request (from the provided local file)
%%   --post [<filename>]         POST request (optional local file)
%%   --copySrc bucket/key        Copy from this source key
%%   --copySrcRange {startIndex}-{endIndex}
%%   --createBucket [<region>]   create-bucket with optional location constraint
%%   --head                      HEAD request
%%   --debug                     enable debug logging
%%  common curl options:
%%   -H 'x-amz-acl: public-read' another way of using canned ACLs
%%   -v                          verbose logging
%% USAGE
s3curl_option_spec() ->
    [{id, undefined, "id", atom, "friendly-name"},
     {key, undefined, "key", string, "id/key are AWSAcessKeyId and Secret (unsafe)"},
     {contentType, undefined, "contentType", {string, "text/plain"}, "set content-type header"},
     {acl, undefined, "acl", string, "public-read     use a 'canned' ACL (x-amz-acl header)"},
     {contentMd5, undefined, "contentMd5", string, "add Content-MD5 header"},
     {calculateContentMd5, undefined, "calculateContentMd5", boolean, "calculate Content-MD5 and add it"},
     {put, undefined, "put", string, "<filename> PUT request (from the provided local file)"},
     {post, undefined, "post", string, "[<filename>] POST request (optional local file)"},
     {copySrc, undefined, "copySrc", string, "bucket/key Copy from this source key"},
     {copySrcRange, undefined, "copySrcRange", string, "{startIndex}-{endIndex}"},
     {createBucket, undefined, "createBucket", string, "[<region>]   create-bucket with optional location constraint"},
     {head, undefined, "head", boolean, "HEAD request"},
     {debug, undefined, "debug", boolean, "enable debug logging"}
    ].
%%   -H 'x-amz-acl: public-read' another way of using canned ACLs
%%   -v                          verbose logging

%% @doc Only covers major options of curl(1). Pull requests to expand
%% coverage would be welcomed.
curl_option_spec() ->
    [{silent, $s, "silent", {boolean, false}, "silent"},
     {verbose, $v, "verbose", {boolean, false}, "verbose"},
     {proxy, $x, "proxy", string, "Proxy host:port"},
     {header, $H, "Header", string, "HTTP Header"},
     {data, $d, "data", string, "body data"}].

cmd_option_spec() ->
    [{debug, $d, "debug", {boolean, false}, "debug"},
     {config, $c, "config", {string, "~/.s3cfg"}, "config file"}].
