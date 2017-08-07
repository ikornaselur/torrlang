-module(trackers).
-export([get_torrent/1, request/2, start/0]).

% Trackers module
%
% Exposes functions for network communications with trackers


start() ->
  inets:start(),
  ssl:start().

get_torrent(Url) ->
  {ok, {{_, 200, _}, _, Payload}} = httpc:request(Url),
  bencoding:decode(Payload).

request({binary, Url}, Info) when is_map(Info) ->
  RequestMap = construct_request_from_info(Info),
  UrlParams = urllib:params_from_map(RequestMap),
  UrlWithParams = lists:append([Url, "?", UrlParams]),
  {ok, {{_, 200, _}, _, Payload}} = httpc:request(UrlWithParams),
  bencoding:decode(Payload).

construct_request_from_info(Info) ->
  % TODO: Update values below to be real data
  maps:from_list([
    {"compact", 1},
    {"downloaded", 0},  % XXX
    {"uploaded", 0},    % XXX
    {"event", "started"},  % XXX
    {"left", maps:get({binary, "length"}, Info)},
    {"peer_id", "torrlang10axeltest10"},  % XXX
    {"port", 6881},  % XXX
    {"info_hash", crypto:hash(sha, bencoding:encode(Info))}
  ]).
