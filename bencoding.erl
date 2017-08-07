-module(bencoding).
-export([decode/1, encode/1]).

% Bencoding module
%
% Exposes functions to decode bencoded strings and
% encode objects into bencoded strings


%%%%%%%%%%%%%%%%
%%% Encoding %%%
%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%
%%% Decoding %%%
%%%%%%%%%%%%%%%%
decode(String) ->
  {Result, []} = bdecode(String),
  Result.


bdecode([H|T]) when H > $0, H =< $9 ->
  % Binary string
  {KeyLength, [_|RestWithKey]} = lists:splitwith(fun(C) -> C =/= $: end, [H|T]),
  {Binary, Rest} = lists:split(list_to_integer(KeyLength), RestWithKey),
  {{binary, Binary}, Rest};

bdecode([$i|T]) ->
  % Integer
  {Number, [_|Rest]} = lists:splitwith(fun(C) -> C =/= $e end, T),
  {list_to_integer(Number), Rest};

bdecode([$l|T]) ->
  % List
  bdecode_list([], T);

bdecode([$d|T]) ->
  % Dictionary
  bdecode_dict(maps:new(), T).


% Parse dictionaries
bdecode_dict(Dict, [$e|Rest]) ->
  % Dictionary ends with $e, so simply return the constructed dictionary
  {Dict, Rest};
bdecode_dict(Dict, Payload) ->
  % Update the Dict with a parse Key + Value
  {Key, RestWithValue} = bdecode(Payload),
  {Value, Rest} = bdecode(RestWithValue),
  bdecode_dict(maps:put(Key, Value, Dict), Rest).


% Parse lists
bdecode_list(List, [$e|Rest]) ->
  % Lists end with $e, so simply return the constructed list
  {List, Rest};
bdecode_list(List, Payload) ->
  {Item, Rest} = bdecode(Payload),
  bdecode_list([Item|List], Rest).
