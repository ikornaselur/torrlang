-module(bencoding_tests).
-import(bencoding, [decode/1, encode/1]).
-include_lib("eunit/include/eunit.hrl").


% Decoding
decoding_numbers_test() ->
  ?assertEqual(decode("i42e"), 42).

decoding_binary_test() ->
  ?assertEqual(decode("4:test"), <<"test">>).

decoding_list_test() ->
  ?assertEqual(decode("l4:testi42ee"), [<<"test">>, 42]).

decoding_list_of_lists_test() ->
  ?assertEqual(decode("lli1ei2eeli3ei4eee"), [[1, 2], [3, 4]]).

decoding_dict_test() ->
  Expected = maps:from_list([{<<"key">>, <<"value">>}]),
  ?assertEqual(decode("d3:key5:valuee"), Expected).

decoding_list_of_dicts_test() ->
  InnerDict1 = #{<<"key1">> => <<"value1">>},
  InnerDict2 = #{<<"key2">> => <<"value2">>},
  Expected = [InnerDict1, InnerDict2],
  ?assertEqual(decode("ld4:key16:value1ed4:key26:value2ee"), Expected).


% Encoding
encoding_numbers_test() ->
  ?assertEqual(encode(42), "i42e").

encoding_binary_test() ->
  ?assertEqual(encode(<<"test">>), "4:test").

encoding_list_test() ->
  ?assertEqual(encode([1, 2, 3]), "li1ei2ei3ee").

encoding_list_of_lists_test() ->
  ?assertEqual(encode([[1, 2], [3, 4]]), "lli1ei2eeli3ei4eee").

encoding_dict_test() ->
  Dict = maps:from_list([{<<"key">>, <<"value">>}]),
  ?assertEqual(encode(Dict), "d3:key5:valuee").

encoding_list_of_dicts_test() ->
  InnerDict1 = #{<<"key1">> => <<"value1">>},
  InnerDict2 = #{<<"key2">> => <<"value2">>},
  List = [InnerDict1, InnerDict2],
  ?assertEqual(encode(List), "ld4:key16:value1ed4:key26:value2ee").

encoding_unknown_type_test() ->
  ?assertException(error, bencoding_unknown_item_to_encode, encode(atom)).
