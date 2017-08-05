-module(torrlang).
-export([test/0, bdecode/1]).

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


bdecode([H|T]) when H > $0, H =< $9 ->
  % Binary string
  {KeyLength, [_|RestWithKey]} = lists:splitwith(fun(C) -> C =/= $: end, [H|T]),
  lists:split(list_to_integer(KeyLength), RestWithKey);
bdecode([H|T]) when H =:= $i ->
  % Integer
  {Number, [_|Rest]} = lists:splitwith(fun(C) -> C =/= $e end, T),
  {list_to_integer(Number), Rest};
bdecode([H|T]) when H =:= $l ->
  % List
  bdecode({list, []}, T);
bdecode([H|T]) when H =:= $d ->
  % Dictionary
  bdecode({dict, maps:new()}, T).

% Parse dictionaries
bdecode({dict, Dict}, [H|Rest]) when H =:= $e ->
  % Dictionary ends with $e, so simply return the constructed dictionary
  {Dict, Rest};
bdecode({dict, Dict}, Payload) ->
  % Update the Dict with a parse Key + Value
  {Key, RestWithValue} = bdecode(Payload),
  {Value, Rest} = bdecode(RestWithValue),
  bdecode({dict, maps:put(Key, Value, Dict)}, Rest);

% Parse lists
bdecode({list, List}, [H|Rest]) when H =:= $e ->
  % Lists end with $e, so simply return the constructed list
  {List, Rest};
bdecode({list, List}, Payload) ->
  {Item, Rest} = bdecode(Payload),
  bdecode({list, [Item|List]}, Rest).


test() ->
  BitTorrentUrl = "https://s3-eu-west-1.amazonaws.com/project-firewater/test/file.txt\?torrent",

  Payload = get_torrent_payload(BitTorrentUrl),
  Decoded = bdecode(Payload),
  io:format("~p~n", [Decoded]).
