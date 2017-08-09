-module(peers).
-export([handshake/1, send_handshake/2]).
-import(config, [config/0]).


send_handshake(Peer, InfoHash) ->
  % Construct the payload
  Handshake = handshake(InfoHash),

  % Open socket
  {ok, Socket} = gen_tcp:connect(
                   maps:get("ip", Peer),
                   maps:get("port", Peer),
                   [binary, {active, false}]
                  ),

  % Send the payload
  gen_tcp:send(Socket, Handshake),

  case gen_tcp:recv(Socket, 0, 5000) of
    {ok, Packet} ->
      io:format("Received packet: ~p~n", [Packet]);
    {error, Why} ->
      io:format("Error: ~p~n", [Why])
  end.


handshake(InfoHash) when is_list(InfoHash) ->
  % <pstrlen><pstr><reserved><info_hash><peer_id>
  Protocol = "BitTorrent protocol",
  ProtocolLength = [length(Protocol)],
  Reserved = [0 || _ <- lists:seq(0, 7)],
  PeerId = maps:get(peer_id, config()),

  lists:concat([ProtocolLength, Protocol, Reserved, InfoHash, PeerId]);
handshake(InfoHash) when is_binary(InfoHash) ->
  handshake(binary_to_list(InfoHash)).
