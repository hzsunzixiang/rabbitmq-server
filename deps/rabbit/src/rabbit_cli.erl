-module(rabbit_cli).

-include_lib("kernel/include/logger.hrl").

-export([main/1]).

main(Args) ->
    case run_cli(Args) of
        ok ->
            erlang:halt();
        {error, ErrorMsg} ->
            io:format(standard_error, "Error: ~ts~n", [ErrorMsg]),
            erlang:halt(1)
    end.

run_cli(Args) ->
    maybe
        Progname = escript:script_name(),
        {ok, ArgMap, RemainingArgs} ?= parse_args(Progname, Args),
        Nodename = lookup_rabbitmq_nodename(ArgMap),
        {ok, _} ?= net_kernel:start(undefined, #{name_domain => shortnames}),
        true ?= net_kernel:connect_node(Nodename),

        {ok, IO} ?= rabbit_cli_io:start_link(),
        try
            Ret = run_command(Nodename, Progname, ArgMap, RemainingArgs, IO),
            io:format("Ret = ~p~n", [Ret])
        after
            rabbit_cli_io:stop(IO)
        end,
        ok
    end.

parse_args(Progname, Args) ->
    Definition = #{arguments =>
                   [#{name => verbose,
                      long => "-verbose",
                      short => $v,
                      action => count,
                      help =>
                      "Be verbose; can be specified multiple times to "
                      "increase verbosity"},
                    #{name => node,
                      long => "-node",
                      short => $n,
                      type => string,
                      nargs => 1,
                      help => "Name of the node to control"}]},
    Options = #{progname => Progname},
    case rabbit_cli_args:parse(Args, Definition, Options) of
        {ok, ArgMap, _CmdPath, _Command, RemainingArgs} ->
            {ok, ArgMap, RemainingArgs};
        {error, _} = Error->
            Error
    end.

lookup_rabbitmq_nodename(#{node := Nodename}) ->
    Nodename1 = complete_nodename(Nodename),
    Nodename1;
lookup_rabbitmq_nodename(_) ->
    GuessedNodename0 = guess_rabbitmq_nodename(),
    GuessedNodename1 = complete_nodename(GuessedNodename0),
    GuessedNodename1.

guess_rabbitmq_nodename() ->
    case net_adm:names() of
        {ok, NamesAndPorts} ->
            Names0 = [Name || {Name, _Port} <- NamesAndPorts],
            Names1 = lists:sort(Names0),
            Names2 = lists:filter(
                       fun
                           ("rabbit" ++ _) -> true;
                           (_) ->             false
                       end, Names1),
            case Names2 of
                [First | _] ->
                    First;
                [] ->
                    "rabbit"
            end;
        {error, address} ->
            "rabbit"
    end.

complete_nodename(Nodename) ->
    case re:run(Nodename, "@", [{capture, none}]) of
        nomatch ->
            {ok, ThisHost} = inet:gethostname(),
            list_to_atom(Nodename ++ "@" ++ ThisHost);
        match ->
            list_to_atom(Nodename)
    end.

%lookup_command_map(Nodename) ->
%    %% Order of operations:
%    %% 1. refresh the cached copy:
%    %%     a. is the node running?
%    %%        yes -> query its uptime
%    %%        no  -> local or remote?
%    %%            local  -> list beam files + file size + last modified date
%    %%            remote -> no refresh possible
%    %%     b. compare uptime to cache date
%    %%        or
%    %%        compare files list to cached files list
%    %%     c. if refresh needed, query command map or (local only) extract it from files list
%    %%
%    %% 2. use cached command map; error out if none
%    %% 
%    %% Or:
%    %% 0. node extracts the command map on startup and stores it persistent_term
%    %% 1. query node for command map with a very short timeout + store result locally
%    %% 2. read local copy
%    %% + we skip the query if `--help` or completion and if there is a local copy
%    case is_node_local(Nodename) of
%        true ->
%            %% Generated local command map.
%            lookup_local_command_map(),
%            {ok, #{}};
%        false ->
%            %% Cache or RPC.
%            {ok, #{}}
%    end.
%
%is_node_local(Nodename) ->
%    case re:run(Nodename, "@(.+)", [{capture, all_but_first, list}]) of
%        {match, ["localhost"]} ->
%            true;
%        {match, [HostPart]} ->
%            ThisHost = inet:gethostname(),
%            HostPart =:= ThisHost;
%        nomatch ->
%            true
%    end.
%
%lookup_local_command_map() ->
%    ScriptDir = filename:dirname(escript:script_name()),
%    io:format("Script = ~p~n", [ScriptDir]),
%    ok.

run_command(Nodename, Progname, ArgMap, RemainingArgs, IO) ->
    try
        erpc:call(
          Nodename,
          rabbit_cli_commands, run_command,
          [Progname, ArgMap, RemainingArgs, IO])
    catch
        error:{erpc, Reason} ->
            {error, Reason}
    end.
