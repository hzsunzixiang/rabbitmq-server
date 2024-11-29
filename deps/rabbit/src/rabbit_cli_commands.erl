-module(rabbit_cli_commands).

-include_lib("kernel/include/logger.hrl").

-export([commands/0, run_command/4]).
-export([list_exchanges/4]).

commands() ->
    %% Extract the commands from module attributes like feature flags and boot
    %% steps.
    #{
      commands =>
      #{
       "list" =>
       #{
         commands =>
         #{"exchanges" =>
           #{
             handler => {?MODULE, list_exchanges}
            }
          }
        }
      }
     }.

run_command(Progname, ArgMap, Args, IO) ->
    Definition = commands(),
    Options = #{progname => Progname},
    case rabbit_cli_args:parse(Args, Definition, Options) of
        {ok, NewArgMap, _CmdPath, Command, RemainingArgs} ->
            ArgMap1 = maps:merge(ArgMap, NewArgMap),
            %% TODO: Put both processes under the rabbit supervision tree.
            RunnerPid = command_runner(
                          Progname, Command, ArgMap1, RemainingArgs, IO),
            RunnerMRef = erlang:monitor(process, RunnerPid),
            receive
                {'DOWN', RunnerMRef, _, _, Reason} ->
                    {ok, Reason}
            end;
        {error, Reason} = Error ->
            ?LOG_ALERT("Error: ~s", [argparse:format_error(Reason)]),
            Error
    end.

command_runner(
  Progname, #{handler := {Mod, Fun}} = _Command, ArgMap, RemainingArgs, IO) ->
    spawn_link(Mod, Fun, [Progname, ArgMap, RemainingArgs, IO]).

list_exchanges(Progname, ArgMap, RemainingArgs, IO) ->
    InfoKeys = rabbit_exchange:info_keys(),
    Fields = lists:map(
               fun
                   (name = Key) ->
                       #{name => Key, type => resource};
                   (type = Key) ->
                       #{name => Key, type => string};
                   (durable = Key) ->
                       #{name => Key, type => boolean};
                   (auto_delete = Key) ->
                       #{name => Key, type => boolean};
                   (internal = Key) ->
                       #{name => Key, type => boolean};
                   (arguments = Key) ->
                       #{name => Key, type => term};
                   (policy = Key) ->
                       #{name => Key, type => string};
                   (user_who_performed_action = Key) ->
                       #{name => Key, type => string};
                   (Key) ->
                       #{name => Key, type => term}
               end, InfoKeys),
    case rabbit_cli_io:start_record_stream(IO, exchanges, Fields, {Progname, ArgMap, RemainingArgs}) of
        {ok, Stream} ->
            Exchanges = rabbit_exchange:list(),
            lists:foreach(
              fun(Exchange) ->
                      Record0 = rabbit_exchange:info(Exchange),
                      Record1 = lists:sublist(Record0, length(Fields)),
                      Record2 = [Value || {_Key, Value} <- Record1],
                      rabbit_cli_io:push_new_record(IO, Stream, Record2)
              end, Exchanges),
            rabbit_cli_io:end_record_stream(IO, Stream);
        {error, _} = Error ->
            Error
    end.
