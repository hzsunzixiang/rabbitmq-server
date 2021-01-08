-module(rabbit_cli_main).

-include("src/sysexits.hrl").

-export([run/2]).

-spec run(string(), iodata()) -> ok.

run(ProgName, RawArgs) ->
    ParserOpts = #{progname => ProgName},
    CommandMods = discover_commands(),
    CommandSpec = collect_args_spec(CommandMods, ParserOpts),

    Ret = try
              %% 1. Argument parsing.
              ParsedArgs = argparse:parse(RawArgs, CommandSpec, ParserOpts),

              %% 2. Output setup.
              rabbit_cli_io:setup(ParsedArgs),
              set_log_level(ParsedArgs),
              logger:debug("argparse:parse/2 -> ~p~n", [ParsedArgs]),

              %% 3. Command execution.
              Result = case ParsedArgs of
                           {ArgMap, PathTo} ->
                               run_handler(
                                 CommandSpec,
                                 ArgMap,
                                 PathTo,
                                 undefined);
                           ArgMap ->
                               run_handler(
                                 CommandSpec,
                                 ArgMap,
                                 {[], CommandSpec},
                                 {CommandMods, #{}})
                       end,

              %% 4. Close output and return exit status.
              rabbit_cli_io:close(),
              Result
          catch
              error:{argparse, Reason} ->
                  Prefixes = maps:get(prefixes, ParserOpts, "-"),
                  case help_requested(Reason, Prefixes) of
                      false ->
                          Error = argparse:format_error(
                                    Reason,
                                    CommandSpec,
                                    ParserOpts),
                          io:format(standard_error, "error: ~s", [Error]);
                      CommandPath ->
                          Help = argparse:help(
                                   CommandSpec,
                                   ParserOpts#{command => tl(CommandPath)}),
                          io:format(standard_error, "~s", [Help])
                  end,
                  {exit, ?EX_USAGE}
          end,
    case Ret of
        ok               -> ok;
        {exit, ExitCode} -> halt(ExitCode)
    end.

discover_commands() ->
    [% TODO: Generate that list.
     rabbit_cli_global_options,
     rabbit_cli_cmd_list_exchanges,
     rabbit_cli_cmd_version
    ].

set_log_level(#{verbose := Verbose}) ->
    if
        Verbose >= 2 -> logger:set_primary_config(level, debug);
        true         -> logger:set_primary_config(level, info)
    end;
set_log_level(ArgMap) when is_map(ArgMap) ->
    logger:set_primary_config(level, notice);
set_log_level({ArgMap, _}) ->
    set_log_level(ArgMap).

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

run_handler(CmdSpec, ArgMap, {Path, #{handler := {Mod, ModFun, Default}}}, _MO) ->
    ArgList = arg_map_to_arg_list(CmdSpec, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Mod, ModFun, ArgList);
run_handler(_CmdSpec, ArgMap, {_Path, #{handler := {Mod, ModFun}}}, _MO) when is_atom(Mod), is_atom(ModFun) ->
    Mod:ModFun(ArgMap);
run_handler(CmdSpec, ArgMap, {Path, #{handler := {Fun, Default}}}, _MO) when is_function(Fun) ->
    ArgList = arg_map_to_arg_list(CmdSpec, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Fun, ArgList);
run_handler(_CmdSpec, ArgMap, {_Path, #{handler := Handler}}, _MO) when is_function(Handler, 1) ->
    Handler(ArgMap);
%% below is compatibility mode: cli/1 behaviour has been removed in 1.1.0, but
%%  is still honoured for existing users
run_handler(CmdSpec, ArgMap, {[], _}, {Modules, Options}) when is_map_key(default, Options) ->
    ArgList = arg_map_to_arg_list(CmdSpec, [], ArgMap, maps:get(default, Options)),
    exec_cli(Modules, CmdSpec, ArgList, Options);
run_handler(CmdSpec, ArgMap, {[], _}, {Modules, Options}) ->
    % {undefined, {ok, Default}, Modules, Options}
    exec_cli(Modules, CmdSpec, [ArgMap], Options).

%% finds first module that exports ctl/1 and execute it
exec_cli([], CmdSpec, _ArgMap, ArgOpts) ->
    %% command not found, let's print usage
    io:format(argparse:help(CmdSpec, ArgOpts));
exec_cli([Mod|Tail], CmdSpec, Args, ArgOpts) ->
    case erlang:function_exported(Mod, cli, length(Args)) of
        true ->
            erlang:apply(Mod, cli, Args);
        false ->
            exec_cli(Tail, CmdSpec, Args, ArgOpts)
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

%% Finds out whether it was --help/-h requested, and exception was thrown due to that
help_requested({unknown_argument, CmdPath, [Prefix, $h]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested({unknown_argument, CmdPath, [Prefix, Prefix, $h, $e, $l, $p]}, Prefixes) ->
    is_prefix(Prefix, Prefixes, CmdPath);
help_requested(_, _) ->
    false.

%% returns CmdPath when Prefix is one of supplied Prefixes
is_prefix(Prefix, Prefixes, CmdPath) ->
    case lists:member(Prefix, Prefixes) of
        true ->
            CmdPath;
        false ->
            false
    end.
