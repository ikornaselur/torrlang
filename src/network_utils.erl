%%%-------------------------------------------------------------------
%% @doc Network utilities.
%% Utilities to help with parsing and working with network requests
%% @end
%%%-------------------------------------------------------------------
-module(network_utils).
-export([receive_message/1, receive_message/2,
         socket_get/2, socket_get/3]).

%%%-------------------------------------------------------------------
%% @doc Receive a peer message from a socket. @end
%%%-------------------------------------------------------------------
receive_message(Socket) ->
  <<Length:32/integer>> = network_utils:socket_get(Socket, 4),
  network_utils:receive_message(Socket, Length).

receive_message(_Socket, 0) ->
  {keep_alive, ""};
receive_message(Socket, 1) ->
  <<RawType:8>> = network_utils:socket_get(Socket, 1),
  Type = case RawType of
           0 -> choke;
           1 -> unchoke;
           2 -> interested;
           3 -> not_interested
         end,
  {Type, ""};
receive_message(Socket, Length) ->
  <<RawType:8>> = network_utils:socket_get(Socket, 1),
  Type = case RawType of
           4 -> have;
           5 -> bitfield;
           6 -> request;
           7 -> piece;
           8 -> cancel
         end,
  RawMessage = network_utils:socket_get(Socket, Length - 1),
  Message = case Type of
              have ->
                <<Index:32/integer>> = RawMessage,
                Index;
              bitfield ->
                [X || <<X:1>> <= RawMessage];
              request ->
                <<Index:32/integer,
                  Begin:32/integer,
                  RequestLength:32/integer>> = RawMessage,
                #{index => Index, piece_begin => Begin, length => RequestLength};
              piece ->
                <<Index:32/integer,
                  Begin:32/integer,
                  Binary/binary>> = RawMessage,
                #{index => Index, piece_begin => Begin, binary => Binary};
              cancel ->
                <<Index:32/integer,
                  Begin:32/integer,
                  RequestLength:32/integer>> = RawMessage,
                #{index => Index, piece_begin => Begin, length => RequestLength}
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
