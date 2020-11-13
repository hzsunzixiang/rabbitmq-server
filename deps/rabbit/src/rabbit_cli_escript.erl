-module(rabbit_cli_escript).

-export([main/1]).

main(Args) ->
    ScriptName = filename:absname(escript:script_name()),
    ScriptDir = filename:dirname(ScriptName),
    ProgName = list_to_atom(filename:basename(ScriptName, ".bat")),

    %% Add RabbitMQ components to Erlang code path.
    ErlLibs = filename:join([ScriptDir, "..", "plugins"]),
    lists:foreach(
      fun(Dir) -> true = code:add_path(Dir) end,
      filelib:wildcard(filename:join([ErlLibs, "*", "ebin"]))),

    %% Run the CLI.
    Mods = [% FIXME: Generate that list.
            rabbit_cli_global_options,
            rabbit_cli_cmd_list_exchanges,
            rabbit_cli_cmd_version
           ],
    cli:run(Args, #{progname => ProgName,
                    modules => Mods,
                    warn => suppress}).
