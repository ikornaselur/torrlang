-module(trackers_tests).
-import(trackers, [parse_peers/1, fold_bytes_to_integer/1]).
-include_lib("eunit/include/eunit.hrl").


fold_bytes_to_integer_single_byte_test() ->
  ?assert(fold_bytes_to_integer([1]) =:= 1).

fold_bytes_to_integer_multiple_bytes_test() ->
  ?assert(fold_bytes_to_integer([1, 1, 1]) =:= 65793).

fold_bytes_to_integer_single_byte_and_zeroes_test() ->
  ?assert(fold_bytes_to_integer([1, 0, 0]) =:= 65536).

fold_bytes_to_integer_empty_test() ->
  ?assert(fold_bytes_to_integer([]) =:= 0).


parse_peers_parses_ip_test() ->
  RawPeers = <<1,2,3,4,0,1>>,
  [Peer] = parse_peers(RawPeers),
  Ip = maps:get("ip", Peer),
  ?assert(Ip =:= [1, 2, 3, 4]).

parse_peers_parses_port_test() ->
  RawPeers = <<1,2,3,4,26,225>>,
  [Peer] = parse_peers(RawPeers),
  Port = maps:get("port", Peer),
  ?assert(Port =:= 6881).

parse_peers_parses_list_of_peers_test() ->
  RawPeers = <<1,2,3,4,0,1,5,6,7,8,0,1>>,
  Peers = parse_peers(RawPeers),
  ?assert(length(Peers) =:= 2).

parse_peers_raises_on_invalid_amount_of_peers_bytes_test() ->
  ?assertException(error, raw_peers_list_wrong_length, parse_peers([1, 2])).
