-module(torrlang).
-export([test/0]).

% Torrlang
%
% A Torrent cleint written in Erlang, as an exercise to learn erlang

url_get(Url) ->
  httpc:request(get, {Url, []}, [], []).


get_torrent_payload(Url) ->
  inets:start(),
  ssl:start(),
  {ok, {{_, 200, _}, _, Body}} = url_get(Url),
  Body.

test() ->
  BitTorrentUrl = "",
  %BitTorrentUrl = "",

  Payload = get_torrent_payload(BitTorrentUrl),
  Decoded = bencoding:decode(Payload),
  io:format("~p~n", [Decoded]).
