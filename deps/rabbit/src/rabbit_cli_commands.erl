-module(rabbit_cli_commands).

-include_lib("kernel/include/logger.hrl").

-export([argparse_def/0, run_command/4]).
-export([list_exchanges/3]).

argparse_def() ->
    %% Extract the commands from module attributes like feature flags and boot
    %% steps.
    #{commands =>
      #{"list" =>
       #{commands =>
         #{"exchanges" =>
           maps:merge(
             rabbit_cli_io:argparse_def(record_stream),
             #{handler => {?MODULE, list_exchanges}})
          }
        }
      }
     }.

run_command(Progname, ArgMap, Args, IO) ->
    Definition = argparse_def(),
    Options = #{progname => Progname},
    case argparse:parse(Args, Definition, Options) of
        {ok, NewArgMap, CmdPath, Command} ->
            ArgMap1 = maps:merge(ArgMap, NewArgMap),
            run_command1(Progname, CmdPath, ArgMap1, Command, IO);
        {error, Reason} = Error ->
            ?LOG_ALERT("Error: ~s", [argparse:format_error(Reason)]),
            Error
    end.

run_command1(Progname, CmdPath, #{help := true}, Command, IO) ->
    rabbit_cli_io:display_help(IO, Progname, CmdPath, Command);
run_command1(Progname, _CmdPath, ArgMap, Command, IO) ->
    %% TODO: Put both processes under the rabbit supervision tree.
    RunnerPid = command_runner(Progname, Command, ArgMap, IO),
    RunnerMRef = erlang:monitor(process, RunnerPid),
    receive
        {'DOWN', RunnerMRef, _, _, Reason} ->
            {ok, Reason}
    end.

command_runner(
  Progname, #{handler := {Mod, Fun}} = _Command, ArgMap, IO) ->
    spawn_link(Mod, Fun, [Progname, ArgMap, IO]).

list_exchanges(Progname, ArgMap, IO) ->
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
    case rabbit_cli_io:start_record_stream(IO, exchanges, Fields, {Progname, ArgMap}) of
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
