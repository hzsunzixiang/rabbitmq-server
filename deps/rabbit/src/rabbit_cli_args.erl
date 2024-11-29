-module(rabbit_cli_args).

-include_lib("kernel/include/logger.hrl").

-export([parse/3]).

parse(Args, Definition, Options) ->
    parse(Args, Definition, Options, []).

parse(Args, Definition, Options, RemainingArgs) ->
    case argparse:parse(Args, Definition, Options) of
        {ok, ArgMap, CmdPath, Command} ->
            RemainingArgs1 = lists:reverse(RemainingArgs),
            {ok, ArgMap, CmdPath, Command, RemainingArgs1};
        {error, {_CmdPath, undefined, Arg, <<>>}} ->
            Args1 = Args -- [Arg],
            RemainingArgs1 = [Arg | RemainingArgs],
            parse(Args1, Definition, Options, RemainingArgs1);
        {error, _} = Error ->
            Error
    end.
