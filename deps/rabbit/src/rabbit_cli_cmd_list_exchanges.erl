-module(rabbit_cli_cmd_list_exchanges).

-export([cli/0]).

-include("rabbit.hrl").

cli() ->
    #{commands => #{
        "exchange" => #{
          commands => #{
            "list" => #{
              help => "Lists exchanges",
              handler => fun list_exchanges/1
             }
           },
          help => "Provides sub-commands to manage exchanges",
          handler => fun(_) -> ok end
         },

        "list_exchanges" => #{
          help => "Lists exchanges",
          handler => fun list_exchanges/1
         }
       }
     }.

list_exchanges(Args) ->
    Node = get_nodename(Args),
    VHost = get_vhost(Args),
    InfoKeys = [name, type, durable, auto_delete, internal, arguments, policy],
    net_kernel:start([foobar, shortnames]),
    io:format("Exchanges on vhost ~p on node ~s~n", [VHost, Node]),
    Ret = rpc:call(Node,
                   rabbit_exchange,
                   info_all,
                   [VHost, InfoKeys]),
    case is_list(Ret) of
        true ->
            rabbit_cli_output:notify(
              {info_table, #{keys => InfoKeys,
                             rows => Ret,
                             callbacks => #{
                               info_key_to_col_title =>
                               fun(S) -> string:titlecase(atom_to_list(S)) end,
                               info_value_to_cell_content =>
                               fun
                                   ({name, #resource{name = <<>>}}) ->
                                       "<empty>";
                                   ({name, #resource{name = Name}}) ->
                                       Name;
                                   ({_, T}) ->
                                       T
                               end
                              }}});
        false ->
            rabbit_cli_output:notify(Ret)
    end.

get_nodename(#{node := Nodename}) ->
    Nodename;
get_nodename(_) ->
    #{nodename := Nodename} = rabbit_env:get_context(),
    Nodename.

get_vhost(#{vhost := VHost}) ->
    VHost;
get_vhost(_) ->
    <<"/">>.
