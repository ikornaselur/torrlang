%%%-------------------------------------------------------------------
%% @doc Bencoding module.
%% Provides functions to encode and decode to and from the "bencoding"
%% encoding used by BitTorrent.
%% @end
%%%-------------------------------------------------------------------
-module(bencoding).
-export([decode/1, encode/1]).

-type torrent_metadata() :: [any()] | integer() | binary() | map().


%%%-------------------------------------------------------------------
%% @doc Encoding of an object.
%% Take an object, of the type torrent_metadata() and encode it into a
%% bencoded string.
%% @end
%%%-------------------------------------------------------------------
-spec encode(torrent_metadata()) -> string().
encode(List) when is_list(List) ->
  Internal = lists:flatmap(fun (Elm) -> encode(Elm) end, List),
  lists:concat(["l", Internal, "e"]);

encode(Dict) when is_map(Dict) ->
  Internal = encode_dict(Dict),
  lists:concat(["d", Internal, "e"]);

encode(Int) when is_integer(Int) ->
  lists:concat(["i", integer_to_list(Int), "e"]);

encode(Bin) when is_binary(Bin) ->
  BinList = binary_to_list(Bin),
  BinaryLength = length(BinList),
  lists:concat([integer_to_list(BinaryLength), ":", BinList]);

encode(_) ->
  erlang:error(bencoding_unknown_item_to_encode).


-spec encode_dict(map()) -> [string()].
encode_dict(Dict) ->
  List = maps:to_list(Dict),
  lists:flatmap(fun ({Key, Value}) -> encode_key_value(Key, Value) end, List).

-spec encode_key_value(binary(), torrent_metadata()) -> [string()].
encode_key_value(Key, Value) ->
  lists:append([encode(Key), encode(Value)]).

%%%-------------------------------------------------------------------
%% @doc Decoding of an object.
%% Take a encoded string, and decode into torrent_metadata()
%% @end
%%%-------------------------------------------------------------------
-spec decode(string()) -> torrent_metadata().
decode(String) ->
  {Result, []} = bdecode(String),
  Result.


-spec bdecode(string()) -> {torrent_metadata(), _}.
bdecode([H|T]) when H > $0, H =< $9 ->
  % Binary string
  {KeyLength, [_|RestWithKey]} = lists:splitwith(fun(C) -> C =/= $: end, [H|T]),
  {Res, Rest} = lists:split(list_to_integer(KeyLength), RestWithKey),
  {list_to_binary(Res), Rest};

bdecode([$i|T]) ->
  % Integer
  {Number, [_|Rest]} = lists:splitwith(fun(C) -> C =/= $e end, T),
  {list_to_integer(Number), Rest};

bdecode([$l|T]) ->
  % List
  {List, Rest} = bdecode_list([], T),
  {lists:reverse(List), Rest};

bdecode([$d|T]) ->
  % Dictionary
  bdecode_dict(maps:new(), T).


-spec bdecode_dict(map(),string()) -> {map(),string()}.
bdecode_dict(Dict, [$e|Rest]) ->
  % Dictionary ends with $e, so simply return the constructed dictionary
  {Dict, Rest};
bdecode_dict(Dict, Payload) ->
  % Update the Dict with a parse Key + Value
  {Key, RestWithValue} = bdecode(Payload),
  {Value, Rest} = bdecode(RestWithValue),
  bdecode_dict(maps:put(Key, Value, Dict), Rest).


-spec bdecode_list([torrent_metadata()], string()) -> {[torrent_metadata()], string()}.
bdecode_list(List, [$e|Rest]) ->
  % Lists end with $e, so simply return the constructed list
  {List, Rest};
bdecode_list(List, Payload) ->
  {Item, Rest} = bdecode(Payload),
  bdecode_list([Item|List], Rest).
