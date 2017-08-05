-module(test).
-export([test/0]).


bdecode([H|T]) when H > $0, H =< $9 ->
  % Binary string
  io:format("Binary string!~n", []),
  {KeyLength, [_|RestWithKey]} = lists:splitwith(fun(C) -> C =/= $: end, [H|T]),
  lists:split(list_to_integer(KeyLength), RestWithKey);
bdecode([H|T]) when H =:= $i ->
  % Integer
  io:format("Integer!~n", []),
  {Number, [_|Rest]} = lists:splitwith(fun(C) -> C =/= $e end, T),
  {list_to_integer(Number), Rest};
bdecode([H|T]) when H =:= $l ->
  % List
  io:format("List!~n", []),
  bdecode({list, []}, T);
bdecode([H|T]) when H =:= $d ->
  % Dictionary
  io:format("Dictionary!~n", []),
  bdecode({dict, maps:new()}, T).

% Parse dictionaries
bdecode({dict, Dict}, [H|Rest]) when H =:= $e ->
  % Dictionary ends with $e, so simply return the constructed dictionary
  {Dict, Rest};
bdecode({dict, Dict}, Payload) ->
  % Update the Dict with a parse Key + Value
  io:format("Parsing dict~n", []),
  {Key, RestWithValue} = bdecode(Payload),
  io:format("Key was: ~p~nNow on to value~n", [Key]),
  {Value, Rest} = bdecode(RestWithValue),
  io:format("Value was: ~p~n", [Value]),
  bdecode({dict, maps:put(Key, Value, Dict)}, Rest);

% Parse lists
bdecode({list, List}, [H|Rest]) when H =:= $e ->
  % Lists end with $e, so simply return the constructed list
  {List, Rest};
bdecode({list, List}, Payload) ->
  {Item, Rest} = bdecode(Payload),
  bdecode({list, [Item|List]}, Rest).

test() ->
  String = "d3:key5:value4:key2d2:ye2:yoe4:listl2:hei25e2:ho2:hiee",
  %String = "d6:lengthi13e4:name4:axele",
  Decoded = bdecode(String),
  io:format("~p~n", [Decoded]).
