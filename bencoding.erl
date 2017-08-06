-module(bencoding).
-export([decode/1, encode/1]).

% Bencoding module
%
% Exposes functions to encode and decode to and from bencoding


%%% Encoding
encode(List) when is_list(List) ->
  Internal = lists:flatmap(fun (Elm) -> encode(Elm) end, List),
  lists:append(["l", Internal, "e"]);
encode(Dict) when is_map(Dict) ->
  Internal = encode_dict(Dict),
  lists:append(["d", Internal, "e"]);
encode(Int) when is_integer(Int) ->
  lists:append(["i", integer_to_list(Int), "e"]);
encode({binary, Bin}) ->
  BinaryLength = length(Bin),
  lists:append([integer_to_list(BinaryLength), ":", Bin]);
encode(_) ->
  erlang:error(bencoding_unknown_item_to_encode).

encode_dict(Dict) ->
  List = maps:to_list(Dict),
  lists:flatmap(fun ({Key, Value}) -> encode_key_value(Key, Value) end, List).
encode_key_value(Key, Value) ->
  lists:append([encode(Key), encode(Value)]).


%%% Decoding
decode([H|T]) when H > $0, H =< $9 ->
  % Binary string
  {KeyLength, [_|RestWithKey]} = lists:splitwith(fun(C) -> C =/= $: end, [H|T]),
  lists:split(list_to_integer(KeyLength), RestWithKey);
decode([H|T]) when H =:= $i ->
  % Integer
  {Number, [_|Rest]} = lists:splitwith(fun(C) -> C =/= $e end, T),
  {list_to_integer(Number), Rest};
decode([H|T]) when H =:= $l ->
  % List
  decode_list([], T);
decode([H|T]) when H =:= $d ->
  % Dictionary
  decode_dict(maps:new(), T).

% Parse dictionaries
decode_dict(Dict, [H|Rest]) when H =:= $e ->
  % Dictionary ends with $e, so simply return the constructed dictionary
  {Dict, Rest};
decode_dict(Dict, Payload) ->
  % Update the Dict with a parse Key + Value
  {Key, RestWithValue} = decode(Payload),
  {Value, Rest} = decode(RestWithValue),
  decode_dict(maps:put(Key, Value, Dict), Rest).

% Parse lists
decode_list(List, [H|Rest]) when H =:= $e ->
  % Lists end with $e, so simply return the constructed list
  {List, Rest};
decode_list(List, Payload) ->
  {Item, Rest} = decode(Payload),
  decode_list([Item|List], Rest).
