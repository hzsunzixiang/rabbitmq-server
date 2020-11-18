-module(rabbit_cli_io_console).
-behaviour(gen_event).

-include_lib("stdout_formatter/include/stdout_formatter.hrl").

-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          setupterm_done = false :: boolean(),
          isatty = #{
            stdin => false,
            stdout => false,
            stderr => false
           } :: #{stdin => boolean(),
                  stdout => boolean(),
                  stderr => boolean()},
          caps = #{} :: map()
         }).

init(_) ->
    State = setup_term(#state{}),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event(
  {log_event, LogEvent, #{formatter := {FModule, FConfig}}},
  State) ->
    io:put_chars(FModule:format(LogEvent, FConfig)),
    {ok, State};
handle_event(
  {info_table, #{keys := Keys, rows := Rows0, callbacks := CBs}},
  State)
  when length(Keys) > 0 ->
    UseColors = use_colors(stdout, State),
    UseLines = use_lines(stdout, State),
    Title = case UseColors of
                true  -> #{title => true};
                false -> #{}
            end,
    Bold = case UseColors of
               true  -> #{fg => green};
               false -> #{}
           end,
    Border = case UseLines of
                 true  -> #{border_drawing => ansi};
                 false -> #{border_drawing => ascii}
             end,
    CB1 = maps:get(info_key_to_col_title, CBs, fun(S) -> S end),
    CB2 = maps:get(info_value_to_cell_content, CBs, fun(S) -> S end),
    TableHeader = #row{cells = lists:map(
                                 CB1,
                                 Keys),
                       props = Title},
    Rows = lists:map(
             fun(Entry) when length(Entry) =:= length(Keys) ->
                     #row{
                        cells = [
                                 %% TODO: Format using callbacks provided by
                                 %% the event emitter.
                                 #paragraph{content = CB2(hd(Entry)),
                                            props = Bold}
                                 | lists:map(CB2, tl(Entry))
                                ]
                       }
             end, Rows0),
    stdout_formatter:display(
      #table{rows = [TableHeader | Rows],
             props = Border#{cell_padding => {0, 1}}}),
    {ok, State};
handle_event(Event, State) ->
    io:format("~p~n", [Event]),
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

setup_term(#state{setupterm_done = true} = State) ->
    State;
setup_term(State) ->
    StdinIsTty = terminfo:isatty(stdin),
    StdoutIsTty = terminfo:isatty(stdout),
    StderrIsTty = terminfo:isatty(stderr),

    Isatty = #{
      stdin => StdinIsTty,
      stdout => StdoutIsTty,
      stderr => StderrIsTty
     },

    Caps = #{
      stdout => query_term_caps(stdout, StdoutIsTty),
      stderr => query_term_caps(stderr, StderrIsTty)
     },

    State#state{
      setupterm_done = true,
      isatty = Isatty,
      caps = Caps
     }.

query_term_caps(FdName, true) ->
    terminfo:setupterm(FdName),
    terminfo:query_all_caps();
query_term_caps(_FdName, false) ->
    #{}.

use_colors(FdName, #state{isatty = Isatty, caps = Caps}) ->
    case maps:get(FdName, Isatty, false) of
        true ->
            FdCaps = maps:get(FdName, Caps, #{}),
            maps:get(colors, FdCaps, 1) > 1;
        false ->
            false
    end.

use_lines(FdName, #state{isatty = Isatty}) ->
    maps:get(FdName, Isatty, false).
