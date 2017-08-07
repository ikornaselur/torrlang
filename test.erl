-module(test).
-export([test/0]).
-include_lib("stdlib/include/assert.hrl").

test() ->
  String = "d3:key5:value4:key2d2:ye2:yoe4:listl2:hei25e2:ho2:hiee",
  io:format("Testing decoding: ~p~n", [String]),
  io:format("~p~n~n", [bencoding:decode(String)]),

  Binary = {binary, "foo"},
  io:format("Testing binary encoding: ~p~n", [Binary]),
  io:format("~p~n~n", [bencoding:encode(Binary)]),

  Int = 123,
  io:format("Testing integer encoding: ~p~n", [Int]),
  io:format("~p~n~n", [bencoding:encode(Int)]),

  List = [Int, Binary],
  io:format("Testing list encoding: ~p~n", [List]),
  io:format("~p~n~n", [bencoding:encode(List)]),

  Dict = maps:from_list([{{binary, "list"}, List}, {{binary, "key"}, {binary, "value"}}]),
  io:format("Testing dict encoding: ~p~n", [Dict]),
  io:format("~p~n~n", [bencoding:encode(Dict)]),

  % Test both directions
  io:format("Testing string -> encoding -> decoding~n", []),
  Decoded = bencoding:decode(String),
  Encoded = bencoding:encode(Decoded),
  io:format("~p~n", [String]),
  io:format("~p~n", [Encoded]),
  Result = case length(String) =:= length(Encoded) of
    true -> ok;
    _ -> fail
  end,
  io:format("Success (checked length): ~p~n", [Result]).
