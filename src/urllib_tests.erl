-module(urllib_tests).
-import(urllib, [url_encode/1]).
-include_lib("eunit/include/eunit.hrl").


url_encode_allowed_chars_test() ->
  ?assert(url_encode("FooBARbaz") =:= "FooBARbaz").

url_encode_disallowed_chars_test() ->
  ?assert(url_encode([1, 100, 200, 250]) =:= "%01d%C8%FA").

url_encode_combined_chars_test() ->
  ?assert(url_encode([1, 100, 200, 250, $a, $b, $c, $d]) =:= "%01d%C8%FAabcd").
