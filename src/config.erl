-module(config).
-export([config/0]).

config() ->
  maps:from_list([
                  {peer_id, "-TO0001-845729384751"},
                  {port, 6881}
                 ]).
