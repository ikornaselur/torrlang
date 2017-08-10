%%%-------------------------------------------------------------------
%% @doc Network utilities.
%% Utilities to help withparsing and working with network requests
%% @end
%%%-------------------------------------------------------------------
-module(network_utils).
-export([receive_message/1, socket_get/2, socket_get/3]).

%%%-------------------------------------------------------------------
%% @doc Receive a peer message from a socket. @end
%%%-------------------------------------------------------------------
receive_message(Socket) ->
  Length = binary:decode_unsigned(socket_get(Socket, 4)),
  receive_message(Socket, Length).

receive_message(_Socket, 0) ->
  io:format("Got keep_alive", []);
receive_message(Socket, 1) ->
  [RawType] = binary_to_list(socket_get(Socket, 1)),
  Type = case RawType of
           0 -> choke;
           1 -> unchoke;
           2 -> interested;
           3 -> not_interested
         end,
  {Type, ""};
receive_message(Socket, Length) ->
  [RawType] = binary_to_list(socket_get(Socket, 1)),
  Type = case RawType of
           4 -> have;
           5 -> bitfield;
           6 -> request;
           7 -> piece;
           8 -> cancel
         end,
  Message = case Type of
              bitfield -> [X || <<X:1>> <= socket_get(Socket, Length - 1)];
              piece ->
                Payload = socket_get(Socket, Length - 1),
                {_IndexAndBegin, File} = lists:split(8, binary_to_list(Payload)),
                File;
              _ -> socket_get(Socket, Length - 1)
            end,
  {Type, Message}.


%%%-------------------------------------------------------------------
%% @doc Get a number of bytes from a socket. @end
%%%-------------------------------------------------------------------
socket_get(Socket, Bytes) ->
  socket_get(Socket, Bytes, 5000).
socket_get(Socket, Bytes, Timeout) ->
  {ok, Result} = gen_tcp:recv(Socket, Bytes, Timeout),
  Result.
