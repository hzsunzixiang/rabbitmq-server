-module(rabbit_cli_main).

-export([run/2]).

-spec run(string(), iodata()) -> ok.

run(_ProgName, RawArgs) ->
    %% 1. Argument parsing.
    CommandMods = discover_commands(),
    CommandMap = collect_args_spec(CommandMods, #{}),
    Ret = argparse:parse(RawArgs, CommandMap),

    %% 2. Output setup.
    rabbit_cli_output:setup(Ret),
    logger:set_primary_config(level, debug),
    %logger:i(),
    logger:debug("argparse:parse/2 -> ~p~n", [Ret]),

    %% 3. Command execution.
    case Ret of
        {ArgMap, PathTo} ->
            run_handler(
              CommandMap, ArgMap, PathTo, undefined);
        ArgMap ->
            %{ maps:find(default, Options), Modules, Options}
            run_handler(
              CommandMap, ArgMap, {[], CommandMap}, {CommandMods, #{}})
    end,


    %% 4. Close output and return exit status.
    rabbit_cli_output:close(),
    ok.

discover_commands() ->
    [% TODO: Generate that list.
     rabbit_cli_global_options,
     rabbit_cli_cmd_list_exchanges,
     rabbit_cli_cmd_version
    ].

%% -------------------------------------------------------------------
%% Copied from `cli` module (argparse 1.1.0).
%% -------------------------------------------------------------------

collect_args_spec(Modules, Options) when is_list(Modules) ->
    lists:foldl(
        fun (Mod, Cmds) ->
                ModCmd =
                try
                    {_, MCmd} = argparse:validate(Mod:cli(), Options),
                    MCmd
                catch
                    _:_ ->
                        %% TODO: Handle error.
                        #{}
                end,

                %% handlers: use first non-empty handler
                Cmds1 =
                case maps:find(handler, ModCmd) of
                    {ok, _Handler} when is_map_key(handler, Cmds) ->
                        %% TODO: Warn about duplicate handlers.
                        Cmds;
                    {ok, Handler} ->
                        Cmds#{handler => Handler};
                    error ->
                        Cmds
                end,

                %% help: concatenate help lines
                Cmds2 =
                case is_map_key(help, ModCmd) of
                    true ->
                       Cmds1#{
                         help =>
                         maps:get(help, ModCmd) ++
                         maps:get(help, Cmds1, "")
                        };
                    false ->
                        Cmds1
                end,

                Cmds3 = merge_arguments(
                          maps:get(arguments, ModCmd, []),
                          Cmds2),
                merge_commands(
                  maps:get(commands, ModCmd, #{}),
                  Mod,
                  Options,
                  Cmds3)
        end, #{}, Modules).

merge_arguments([], Existing) ->
    Existing;
merge_arguments(Args, Existing) ->
    ExistingArgs = maps:get(arguments, Existing, []),
    Existing#{arguments => ExistingArgs ++ Args}.

%% argparse accepts a map of commands, which means, commands names
%%  can never clash. Yet for cli it is possible when multiple modules
%%  export command with the same name. For this case, skip duplicate
%%  command names, emitting a warning.
merge_commands(Cmds, Mod, Options, Existing) ->
    MergedCmds = maps:fold(
        fun (Name, Cmd, Acc) ->
                case maps:find(Name, Acc) of
                    error ->
                        %% merge command with name Name into Acc-umulator
                        Acc#{
                          Name =>
                          create_handlers(
                            Mod, Name, Cmd, maps:find(default, Options))
                         };
                    {ok, _Another} ->
                        %% TODO: Warn about command conflict.
                        Acc
                end
        end, maps:get(commands, Existing, #{}), Cmds),
    Existing#{commands => MergedCmds}.

%% Descends into sub-commands creating handlers where applicable
create_handlers(Mod, _CmdName, Cmd0, DefaultTerm) ->
    Handler =
    case maps:find(handler, Cmd0) of
        % TODO:
        %error ->
        %    make_handler(CmdName, Mod, DefaultTerm);
        %{ok, optional} ->
        %    make_handler(CmdName, Mod, DefaultTerm);
        {ok, Existing} ->
            Existing
    end,
    %%
    Cmd = Cmd0#{handler => Handler},
    case maps:find(commands, Cmd) of
        error ->
            Cmd;
        {ok, Sub} ->
            NewCmds = maps:map(
                        fun (CN, CV) ->
                                create_handlers(Mod, CN, CV, DefaultTerm)
                        end,
                        Sub),
            Cmd#{commands => NewCmds}
    end.

run_handler(CmdMap, ArgMap, {Path, #{handler := {Mod, ModFun, Default}}}, _MO) ->
    ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Mod, ModFun, ArgList);
run_handler(_CmdMap, ArgMap, {_Path, #{handler := {Mod, ModFun}}}, _MO) when is_atom(Mod), is_atom(ModFun) ->
    Mod:ModFun(ArgMap);
run_handler(CmdMap, ArgMap, {Path, #{handler := {Fun, Default}}}, _MO) when is_function(Fun) ->
    ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Fun, ArgList);
run_handler(_CmdMap, ArgMap, {_Path, #{handler := Handler}}, _MO) when is_function(Handler, 1) ->
    Handler(ArgMap);
%% below is compatibility mode: cli/1 behaviour has been removed in 1.1.0, but
%%  is still honoured for existing users
run_handler(CmdMap, ArgMap, {[], _}, {Modules, Options}) when is_map_key(default, Options) ->
    ArgList = arg_map_to_arg_list(CmdMap, [], ArgMap, maps:get(default, Options)),
    exec_cli(Modules, CmdMap, ArgList, Options);
run_handler(CmdMap, ArgMap, {[], _}, {Modules, Options}) ->
    % {undefined, {ok, Default}, Modules, Options}
    exec_cli(Modules, CmdMap, [ArgMap], Options).

%% finds first module that exports ctl/1 and execute it
exec_cli([], CmdMap, _ArgMap, ArgOpts) ->
    %% command not found, let's print usage
    io:format(argparse:help(CmdMap, ArgOpts));
exec_cli([Mod|Tail], CmdMap, Args, ArgOpts) ->
    case erlang:function_exported(Mod, cli, length(Args)) of
        true ->
            erlang:apply(Mod, cli, Args);
        false ->
            exec_cli(Tail, CmdMap, Args, ArgOpts)
    end.

%% Given command map, path to reach a specific command, and a parsed argument
%%  map, returns a list of arguments (effectively used to transform map-based
%%  callback handler into positional).
arg_map_to_arg_list(Command, Path, ArgMap, Default) ->
    AllArgs = collect_arguments(Command, Path, []),
    [maps:get(Arg, ArgMap, Default) || #{name := Arg} <- AllArgs].

%% recursively descend into Path, ignoring arguments with duplicate names
collect_arguments(Command, [], Acc) ->
    Acc ++ maps:get(arguments, Command, []);
collect_arguments(Command, [H|Tail], Acc) ->
    Args = maps:get(arguments, Command, []),
    Next = maps:get(H, maps:get(commands, Command, H)),
    collect_arguments(Next, Tail, Acc ++ Args).
