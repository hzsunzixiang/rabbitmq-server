-module(rabbit_cli_io).

-include_lib("kernel/include/logger.hrl").

-include_lib("rabbit_common/include/resource.hrl").

-export([start_link/0,
         stop/1,
         argparse_def/1,
         display_help/4,
         start_record_stream/4,
         push_new_record/3,
         end_record_stream/2]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(?MODULE, {record_streams = #{}}).

start_link() ->
    gen_server:start_link(rabbit_cli_io, none, []).

stop(IO) ->
    MRef = erlang:monitor(process, IO),
    _ = gen_server:call(IO, stop),
    receive
        {'DOWN', MRef, _, _, _Reason} ->
            ok
    end.

argparse_def(record_stream) ->
    #{arguments =>
      [
       #{name => output,
         long => "-output",
         short => $o,
         type => string,
         nargs => 1,
         help => "Write output to file <FILE>"},
       #{name => format,
         long => "-format",
         short => $f,
         type => {atom, [plain, json]},
         nargs => 1,
         help => "Format output acccording to <FORMAT>"}
      ]
     }.

display_help(IO, Progname, CmdPath, ArgparseDef) ->
    gen_server:cast(IO, {?FUNCTION_NAME, Progname, CmdPath, ArgparseDef}).

start_record_stream(IO, Name, Fields, {_Progname, ArgMap} = ProgAndArgs)
  when is_pid(IO) andalso
       is_atom(Name) andalso
       is_map(ArgMap) ->
    gen_server:call(IO, {?FUNCTION_NAME, Name, Fields, ProgAndArgs}).

push_new_record(IO, #{name := Name}, Record) ->
    gen_server:cast(IO, {?FUNCTION_NAME, Name, Record}).

end_record_stream(IO, #{name := Name}) ->
    gen_server:cast(IO, {?FUNCTION_NAME, Name}).

init(_Args) ->
    process_flag(trap_exit, true),
    State = #?MODULE{},
    {ok, State}.

handle_call(
  {start_record_stream, Name, Fields, {_Progname, _ArgMap}},
  From,
  #?MODULE{record_streams = Streams} = State) ->
    Stream = #{name => Name, fields => Fields},
    gen_server:reply(From, {ok, Stream}),

    FieldNames = [atom_to_list(FieldName)
                  || #{name := FieldName} <- Fields],
    Header = string:join(FieldNames, "\t"),
    io:format("~ts~n", [Header]),

    Streams1 = Streams#{Name => Stream},
    State1 = State#?MODULE{record_streams = Streams1},
    {noreply, State1};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(
  {display_help, Progname, CmdPath, ArgparseDef},
  State) ->
    Options = #{progname => Progname,
                command => CmdPath},
    Help = argparse:help(ArgparseDef, Options),
    io:format("~s~n", [Help]),
    {noreply, State};
handle_cast(
  {push_new_record, Name, Record},
  #?MODULE{record_streams = Streams} = State) ->
    #{fields := Fields} = maps:get(Name, Streams),
    Values = format_fields(Fields, Record),
    Line = string:join(Values, "\t"),
    io:format("~ts~n", [Line]),
    {noreply, State};
handle_cast({end_record_stream, _Name}, State) ->
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_fields(Fields, Values) ->
    format_fields(Fields, Values, []).

format_fields([#{type := string} | Rest1], [Value | Rest2], Acc) ->
    String = io_lib:format("~ts", [Value]),
    Acc1 = [String | Acc],
    format_fields(Rest1, Rest2, Acc1);
format_fields([#{type := integer} | Rest1], [Value | Rest2], Acc) ->
    String = io_lib:format("~b", [Value]),
    Acc1 = [String | Acc],
    format_fields(Rest1, Rest2, Acc1);
format_fields([#{type := boolean} | Rest1], [Value | Rest2], Acc) ->
    String = io_lib:format("~ts", [if Value -> "☑"; true -> "☐" end]),
    Acc1 = [String | Acc],
    format_fields(Rest1, Rest2, Acc1);
format_fields([#{type := resource} | Rest1], [Value | Rest2], Acc) ->
    #resource{name = Name} = Value,
    String = io_lib:format("~ts", [Name]),
    Acc1 = [String | Acc],
    format_fields(Rest1, Rest2, Acc1);
format_fields([#{type := term} | Rest1], [Value | Rest2], Acc) ->
    String = io_lib:format("~0p", [Value]),
    Acc1 = [String | Acc],
    format_fields(Rest1, Rest2, Acc1);
format_fields([], [], Acc) ->
    lists:reverse(Acc).

%-export([setup/1,
%         close/0,
%         notify/1,
%         sync_notify/1,
%         log/2]).
%
%-define(EVENT_MGR_REF, ?MODULE).
%
%setup(Args) ->
%    case gen_event:start_link({local, ?EVENT_MGR_REF}, []) of
%        {ok, Pid} ->
%            ok = gen_event:add_sup_handler(
%                   Pid,
%                   rabbit_cli_io_console,
%                   Args),
%            ok = logger:add_handler(
%                   ?EVENT_MGR_REF, ?MODULE, #{}),
%            ok = logger:remove_handler(default),
%            %% TODO: Register output for Erlang Logger.
%            {ok, Pid}
%    end.
%
%close() ->
%    gen_event:stop(?EVENT_MGR_REF).
%
%notify(Event) ->
%    gen_event:notify(?EVENT_MGR_REF, Event).
%
%sync_notify(Event) ->
%    gen_event:sync_notify(?EVENT_MGR_REF, Event).
%
%log(LogEvent, Config) ->
%    sync_notify({log_event, LogEvent, Config}).
