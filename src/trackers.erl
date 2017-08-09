%%%-------------------------------------------------------------------
%% @doc Trackers module.
%% Network communications to trackers and utility functions for parsing
%% trackers respones
%% @end
%%%-------------------------------------------------------------------
-module(trackers).
-export([get_torrent/1, request/2, parse_peers/1, fold_bytes_to_integer/1, start/0]).

start() ->
  inets:start(),
  ssl:start().

get_torrent(Url) ->
  {ok, {{_, 200, _}, _, Payload}} = httpc:request(Url),
  bencoding:decode(Payload).

request(Url, Info) when is_list(Url) andalso is_map(Info) ->
  RequestMap = construct_request_from_info(Info),
  UrlParams = urllib:params_from_map(RequestMap),
  UrlWithParams = lists:append([Url, "?", UrlParams]),
  {ok, {{_, 200, _}, _, Payload}} = httpc:request(UrlWithParams),
  bencoding:decode(Payload);
request(Url, Info) when is_binary(Url) ->
  request(binary_to_list(Url), Info).

construct_request_from_info(Info) ->
  % TODO: Update values below to be real data
  maps:from_list([
    {"compact", 1},
    {"downloaded", 0},  % XXX
    {"uploaded", 0},    % XXX
    {"event", "started"},  % XXX
    {"left", maps:get(<<"length">>, Info)},
    {"peer_id", "torrlang10axeltest10"},  % XXX
    {"port", 6881},  % XXX
    {"info_hash", crypto:hash(sha, bencoding:encode(Info))}
  ]).


%%%-------------------------------------------------------------------
%% @doc Parse a big-endian list of bytes into an integer. @end
%%%-------------------------------------------------------------------
-spec fold_bytes_to_integer([byte()]) -> integer().
fold_bytes_to_integer(Bytes) ->
  fold_bytes_to_integer(lists:reverse(Bytes), 1).
-spec fold_bytes_to_integer([byte()], integer()) -> integer().
fold_bytes_to_integer([], _) ->
  0;
fold_bytes_to_integer([Byte|Rest], Mult) ->
  Mult * Byte + fold_bytes_to_integer(Rest, Mult * 256).


%%%-------------------------------------------------------------------
%% @doc Parse compact peers response.
%% Parse a list of peers from the tracker in a compact form, where each
%% peer is a 6 byte section consisting of 4 bytes for the IP address and
%% 2 bytes for the port.
%% @end
%%%-------------------------------------------------------------------
-spec parse_peers([byte()]) -> [#{string()=>[byte()] | string()}].
parse_peers([]) -> [];
parse_peers(Peers) when is_binary(Peers) ->
  parse_peers(binary_to_list(Peers));
parse_peers(Peers) when length(Peers) rem 6 =:= 0 ->
  {RawPeer, Rest} = lists:split(6, Peers),
  {Ip, Port} = lists:split(4, RawPeer),
  Peer = maps:from_list([{"ip", Ip}, {"port", fold_bytes_to_integer(Port)}]),
  [Peer|parse_peers(Rest)];
parse_peers(_) -> erlang:error(raw_peers_list_wrong_length).
