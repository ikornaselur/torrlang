-module(torrlang).
-export([test/0]).

% Torrlang
%
% A Torrent client written in Erlang, as an exercise to learn erlang

test() ->
  trackers:start(),
  BitTorrentUrl = "",

  io:format("Fetching ~p~n", [BitTorrentUrl]),
  Torrent = trackers:get_torrent(BitTorrentUrl),

  Info = maps:get({binary, "info"}, Torrent),
  AnnounceUrl = maps:get({binary, "announce"}, Torrent),

  io:format("Querying tracker..~n", []),
  Response = trackers:request(AnnounceUrl, Info),

  io:format("Tracker response:~n ~p~n", [Response]).
