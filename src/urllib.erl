%%%-------------------------------------------------------------------
%% @doc Urllib module.
%% Utility functions to work with URLs, such as to construct url safe
%% parameters in a get request
%% @end
%%%-------------------------------------------------------------------
-module(urllib).
-export([url_encode/1, params_from_map/1]).


%%%-------------------------------------------------------------------
%% @doc Construct url params from a Map.
%% Take a key/value map to generate url params.
%% For example, take #{"foo" => "bar"} and turn it into "foo=bar".
%% @end
%%%-------------------------------------------------------------------
-spec params_from_map(map()) -> string().
params_from_map(Map) when is_map(Map) ->
  List = lists:map(fun ({Key, Value}) -> url_param_key(Key, Value) end, maps:to_list(Map)),
  Joined = lists:join("&", List),
  lists:flatten(Joined).

%%%-------------------------------------------------------------------
%% @doc Urlencode strings.
%% Takes urls and encodes the "url unsafe characters" to be url safe.
%% Heavily inspired by https://goo.gl/9ENw74
%% @end
%%%-------------------------------------------------------------------
-spec url_encode(string()) -> string().
url_encode(List) when is_list(List) ->
  AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",
  F = fun (E) ->
          case sets:is_element(E, sets:from_list(AllowedChars)) of
            true -> E;
            false -> lists:concat(["%", io_lib:format("~2.16.0B", [E])])
          end
      end,
  lists:flatten([F(E) || E <- List]);
url_encode(Binary) when is_binary(Binary) ->
  url_encode(binary_to_list(Binary)).

-spec url_param_key(string(), string()) -> string().
url_param_key(Key, Value) ->
  SafeKey = url_encode(Key),
  SafeValue = case Value of
                KeyValue when is_list(KeyValue) orelse is_binary(KeyValue) ->
                  url_encode(KeyValue);
                KeyValue when is_integer(KeyValue) ->
                  integer_to_list(KeyValue);
                _ -> erlang:error(unknown_type_of_key)
              end,
  lists:append([SafeKey, "=", SafeValue]).
