-module(config).
-export([config/0]).

config() ->
  #{ peer_id => "-TO0001-845729384751",
     port => 6881 }.
