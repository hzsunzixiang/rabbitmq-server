-module(rabbit_cli_io).

-export([setup/1,
         close/0,
         notify/1,
         sync_notify/1,
         log/2]).

-define(EVENT_MGR_REF, ?MODULE).

setup(Args) ->
    case gen_event:start_link({local, ?EVENT_MGR_REF}, []) of
        {ok, Pid} ->
            ok = gen_event:add_sup_handler(
                   Pid,
                   rabbit_cli_io_console,
                   Args),
            ok = logger:add_handler(
                   ?EVENT_MGR_REF, ?MODULE, #{}),
            ok = logger:remove_handler(default),
            %% TODO: Register output for Erlang Logger.
            {ok, Pid}
    end.

close() ->
    gen_event:stop(?EVENT_MGR_REF).

notify(Event) ->
    gen_event:notify(?EVENT_MGR_REF, Event).

sync_notify(Event) ->
    gen_event:sync_notify(?EVENT_MGR_REF, Event).

log(LogEvent, Config) ->
    sync_notify({log_event, LogEvent, Config}).
