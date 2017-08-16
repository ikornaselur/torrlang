-module(urllib_tests).
-import(urllib, [url_encode/1, params_from_map/1]).
-include_lib("eunit/include/eunit.hrl").


url_encode_allowed_chars_test() ->
  ?assertEqual(url_encode("FooBARbaz"), "FooBARbaz").

url_encode_disallowed_chars_test() ->
  ?assertEqual(url_encode([1, 100, 200, 250]), "%01d%C8%FA").

url_encode_combined_chars_test() ->
  ?assertEqual(url_encode([1, 100, 200, 250, $a, $b, $c, $d]), "%01d%C8%FAabcd").

url_encode_binary_test() ->
  ?assertEqual(url_encode(<<$a, $b, $c, $d>>), "abcd").

params_from_map_test() ->
  Map = maps:from_list([{"key", "value"}]),
  ?assertEqual(params_from_map(Map), "key=value").

params_from_map_invalid_value_test() ->
  Map = maps:from_list([{"key", atom}]),
  ?assertException(error, unknown_type_of_key, params_from_map(Map)).
