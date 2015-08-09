-module(s5c_config).

-export([get/1, generate/0]).

-define(CONFIGFILE, ".s5c").

-spec get(term()) -> term().
get(Key) ->
    maybe_init(),
    [{Key, Val}] = ets:lookup(?MODULE, Key),
    Val.

get_all() ->
    maybe_init(),
    ets:tab2list(?MODULE).
    
maybe_init() ->
    %% Load config from file to ets only when ets table is not present    
    case ets:info(?MODULE) of
        undefined ->
            _Tid = ets:new(?MODULE, [named_table, public, ordered_set]),
            {ok, Config} = file:consult(config_file()),
            S5CConfig = proplists:get_value(s5c, Config),
            _ = ets:insert_new(?MODULE, S5CConfig);
        _ ->
            noop
    end.

generate() ->
    String =
        "{s5c, [\n"
        " {local, {\"admin-key\", \"admin-secret\"}},\n"
        " {s3, {\"doodle\", \"boodle\"}}\n"
        "]}.\n",
    %% lager:info("opening: ~p", [?CONFIGFILE]),
    ConfigFile = config_file(),
    case file:read_file_info(ConfigFile) of
        {error, enoent} ->
            %% ok = file:write_file("/Users/kuenishi/.s5c", String).
            {ok, IoDevice} = file:open("/Users/kuenishi/.s5c", [write]),
            ok = file:write(IoDevice, String),
            ok = file:close(IoDevice);
        {ok, _} ->
            io:format("~p~n", [get_all()])
    end.

config_file() ->
    filename:join([os:getenv("HOME"), ?CONFIGFILE]).
