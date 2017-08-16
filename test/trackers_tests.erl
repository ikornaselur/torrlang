-module(trackers_tests).
-import(trackers, [parse_peers/1]).
-include_lib("eunit/include/eunit.hrl").


parse_peers_parses_ip_test() ->
  RawPeers = <<1,2,3,4,0,1>>,
  [Peer] = parse_peers(RawPeers),
  Ip = maps:get(ip, Peer),
  ?assertEqual(Ip, {1,2,3,4}).

parse_peers_parses_port_test() ->
  RawPeers = <<1,2,3,4,26,225>>,
  [Peer] = parse_peers(RawPeers),
  Port = maps:get(port, Peer),
  ?assertEqual(Port, 6881).

parse_peers_parses_list_of_peers_test() ->
  RawPeers = <<1,2,3,4,9,8,5,6,7,8,9,9>>,
  [Peer1, Peer2] = parse_peers(RawPeers),
  ?assertEqual(maps:get(ip, Peer1), {1, 2, 3, 4}),
  ?assertEqual(maps:get(ip, Peer2), {5, 6, 7, 8}),
  ?assertEqual(maps:get(port, Peer1), 2312),
  ?assertEqual(maps:get(port, Peer2), 2313).
