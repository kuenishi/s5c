# s5c
s3 and s2

An escript

Build
-----

    $ rebar3 escriptize

Configure
---------

    $ _build/default/bin/s5c generate
    $ cat ~/.s5c
    {s5c, [
      {local, {"admin-key", "admin-secret"}}
    ]}.

Run
---

```
$ _build/default/bin/s5c curl --id local -- -x localhost:8080 http://spam.s3.amazonaws.com/a
spam[{proxy,"localhost:8080"},{silent,false},{verbose,false}]GET


Sun, 09 Aug 2015 15:11:23 GMT
/spam/a
Resp: <<"HTTP/1.1 200 OK\r\nx-amz-meta-s3cmd-attrs: uid:501/gname:staff/uname:kuenishi/gid:20/mode:33188/mtime:1436859412/atime:1439131496/md5:60b725f10c9c85c70d97880dfe8191b3/ctime:1436859412\r\nServer: Riak CS\r\nLast-Modified: Sun, 09 Aug 2015 14:45:00 GMT\r\nETag: \"60b725f10c9c85c70d97880dfe8191b3\"\r\nDate: Sun, 09 Aug 2015 15:11:23 GMT\r\nContent-Type: binary/octet-stream\r\nContent-Length: 2\r\n\r\n">>
here>> <<"a\n">>here>> 2{request,"localhost",8080,http,false,false,get,"spam","/a","/a",
         [{'Authorization',["AWS ","admin-key",58,
                            "gLahRv7szhIaGoWQtzAWEIWZz7c="]},
          {date,"Sun, 09 Aug 2015 15:11:23 GMT"},
          {host,"spam.s3.amazonaws.com"}],
         undefined} => {response,200,<<"1.1">>,<<"OK">>,
                                 [{"Content-Length","2"},
                                  {"Content-Type","binary/octet-stream"},
                                  {"Date","Sun, 09 Aug 2015 15:11:23 GMT"},
                                  {"ETag",
                                   "\"60b725f10c9c85c70d97880dfe8191b3\""},
                                  {"Last-Modified",
                                   "Sun, 09 Aug 2015 14:45:00 GMT"},
                                  {"Server","Riak CS"},
                                  {"x-amz-meta-s3cmd-attrs",
                                   "uid:501/gname:staff/uname:kuenishi/gid:20/mode:33188/mtime:1436859412/atime:1439131496/md5:60b725f10c9c85c70d97880dfe8191b3/ctime:1436859412"}],
                                 {raw,<<"a\n">>}}
a
ok
```
