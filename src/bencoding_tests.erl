-module(bencoding_tests).
-import(bencoding, [decode/1, encode/1]).
-include_lib("eunit/include/eunit.hrl").


% Decoding
decoding_numbers_test() ->
  ?assert(decode("i42e") =:= 42).

decoding_binary_test() ->
  ?assert(decode("4:test") =:= {binary, "test"}).

decoding_list_test() ->
  ?assert(decode("l4:testi42ee") =:= [{binary, "test"}, 42]).

decoding_list_of_lists_test() ->
  ?assert(decode("lli1ei2eeli3ei4eee") =:= [[1, 2], [3, 4]]).

decoding_dict_test() ->
  Expected = maps:from_list([{{binary, "key"}, {binary, "value"}}]),
  ?assert(decode("d3:key5:valuee") =:= Expected).

decoding_list_of_dicts_test() ->
  InnerDict1 = maps:from_list([{{binary, "key1"}, {binary, "value1"}}]),
  InnerDict2 = maps:from_list([{{binary, "key2"}, {binary, "value2"}}]),
  Expected = [InnerDict1, InnerDict2],
  ?assert(decode("ld4:key16:value1ed4:key26:value2ee") =:= Expected).


% Encoding
encoding_numbers_test() ->
  ?assert(encode(42) =:= "i42e").

encoding_binary_test() ->
  ?assert(encode({binary, "test"}) =:= "4:test").

encoding_list_test() ->
  ?assert(encode([1, 2, 3]) =:= "li1ei2ei3ee").

encoding_list_of_lists_test() ->
  ?assert(encode([[1, 2], [3, 4]]) =:= "lli1ei2eeli3ei4eee").

encoding_dict_test() ->
  Dict = maps:from_list([{{binary, "key"}, {binary, "value"}}]),
  ?assert(encode(Dict) =:= "d3:key5:valuee").

encoding_list_of_dicts_test() ->
  InnerDict1 = maps:from_list([{{binary, "key1"}, {binary, "value1"}}]),
  InnerDict2 = maps:from_list([{{binary, "key2"}, {binary, "value2"}}]),
  List = [InnerDict1, InnerDict2],
  ?assert(encode(List) =:= "ld4:key16:value1ed4:key26:value2ee").
