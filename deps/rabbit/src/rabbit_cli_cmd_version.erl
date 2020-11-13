-module(rabbit_cli_cmd_version).

-export([cli/0]).

cli() ->
    #{commands => #{
        "version" => #{
          help => "Shows the version of RabbitMQ and exit",
          handler => fun show_version/1
         }
       }
     }.

show_version(_) ->
    ok = application:load(rabbit),
    {ok, Version} = application:get_key(rabbit, vsn),
    io:format("~s~n", [Version]).
