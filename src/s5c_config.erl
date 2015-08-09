-module(s5c_config).

-export([get/1]).

-define(CONFIGFILE, "~/.s5c").

-spec get(term()) -> term().
get(Key) ->
    maybe_init(),
    [Val] = ets:lookup(?MODULE, Key),
    Val.
    
maybe_init() ->
    %% Load config from file to ets only when ets table is not present    
    case ets:info(?MODULE) of
        undefined ->
            _ = ets:new(?MODULE, [public]),
            {ok, Config} = file:consult(?CONFIGFILE),
            _ = ets:insert(?MODULE, Config);
        _ ->
            noop
    end.
