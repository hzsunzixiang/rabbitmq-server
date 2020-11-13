-module(rabbit_cli_global_options).

-export([cli/0]).

cli() ->
    #{arguments =>
      [
       #{name => node,
         short => $n,
         long => "-node",
         type => string,
         action => store},

       #{name => vhost,
         short => $v,
         long => "-vhost",
         type => binary,
         action => store},

       #{name => verbose,
         short => $V,
         long => "-verbose",
         action => count}
      ]}.
