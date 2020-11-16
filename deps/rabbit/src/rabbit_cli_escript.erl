-module(rabbit_cli_escript).

-export([main/1]).

main(Args) ->
    ScriptName = filename:absname(escript:script_name()),
    ScriptDir = filename:dirname(ScriptName),
    ProgName = list_to_atom(filename:basename(ScriptName, ".bat")),

    %% Add RabbitMQ components to Erlang code path.
    ErlLibs = case filename:basename(ScriptDir) of
                  "scripts" -> filename:join([ScriptDir, "..", ".."]);
                  _ ->         filename:join([ScriptDir, "..", "plugins"])
              end,
    lists:foreach(
      fun(Dir) -> true = code:add_path(Dir) end,
      filelib:wildcard(filename:join([ErlLibs, "*", "ebin"]))),

    %% Run the CLI.
    case rabbit_cli_main:run(ProgName, Args) of
        ok -> halt();
        _  -> halt(1)
    end.
