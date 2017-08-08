-module(urllib).
-export([url_encode/1, params_from_map/1]).

% Urllib module
%
% Utils module for url items, such as to create urlsafe parameters


params_from_map(Map) when is_map(Map) ->
  List = lists:map(fun ({Key, Value}) -> url_param_key(Key, Value) end, maps:to_list(Map)),
  Joined = lists:join("&", List),
  lists:flatten(Joined).

% Heavily inspired from https://goo.gl/9ENw74
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
