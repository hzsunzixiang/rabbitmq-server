-module(rabbit_cli_output_console).
-behaviour(gen_event).

-include_lib("stdout_formatter/include/stdout_formatter.hrl").

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

init(_) ->
    {ok, undefined}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event(Event, State) ->
    io:format("~p~n", [Event]),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
