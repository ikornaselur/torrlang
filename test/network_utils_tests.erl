-module(network_utils_tests).
-import(network_utils, [receive_message/1, receive_message/2]).
-include_lib("eunit/include/eunit.hrl").


receive_message_test_() ->
  {foreach,
   fun() ->
       meck:new(network_utils, [passthrough])
   end,
   fun(_) ->
       meck:unload(network_utils)
   end,
   [{"Length of message is parsed correctly",
     fun() ->
         meck:sequence(network_utils, socket_get, 2, [<<0,0,0,1>>, <<1,1,1,1>>]),
         meck:expect(network_utils, receive_message, 2, ["", ""]),

         receive_message(mock_socket),
         receive_message(mock_socket),

         ?assert(meck:called(network_utils, receive_message, [mock_socket, 1])),
         ?assert(meck:called(network_utils, receive_message, [mock_socket, 16843009]))
     end},
    {"Empty payload parsed as keep_alive",
     fun() ->
         {Type, _Message} = receive_message(mock_socket, 0),
         ?assertEqual(Type, keep_alive)
     end},
    {"Type 'choke' is parsed correctly",
     fun() ->
         meck:expect(network_utils, socket_get, 2, <<0>>),
         {Type, _Message} = receive_message(mock_socket, 1),
         ?assertEqual(Type, choke)
     end},
    {"Type 'unchoke' is parsed correctly",
     fun() ->
         meck:expect(network_utils, socket_get, 2, <<1>>),
         {Type, _Message} = receive_message(mock_socket, 1),
         ?assertEqual(Type, unchoke)
     end},
    {"Type 'interested' is parsed correctly",
     fun() ->
         meck:expect(network_utils, socket_get, 2, <<2>>),
         {Type, _Message} = receive_message(mock_socket, 1),
         ?assertEqual(Type, interested)
     end},
    {"Type 'not_interested' is parsed correctly",
     fun() ->
         meck:expect(network_utils, socket_get, 2, <<3>>),
         {Type, _Message} = receive_message(mock_socket, 1),
         ?assertEqual(Type, not_interested)
     end},
    {"Type 'have' is parsed correctly",
     fun() ->
         meck:sequence(network_utils, socket_get, 2, [<<4>>, <<0,0,0,5>>]),
         {Type, Message} = receive_message(mock_socket, 5),
         ?assertEqual(Type, have),
         ?assertEqual(Message, 5)
     end},
    {"Type 'bitfield' is parsed correctly",
     fun() ->
         % Integer 123 represents the bitfield [0,1,1,1,1,0,1,1]
         meck:sequence(network_utils, socket_get, 2, [<<5>>, <<123>>]),
         {Type, Message} = receive_message(mock_socket, 2),
         ?assertEqual(Type, bitfield),
         ?assertEqual(Message, [0,1,1,1,1,0,1,1])
     end},
    {"Type 'request' is parsed correctly",
     fun() ->
         Index = 1,
         Begin = 2,
         Length = 123,
         MockPayload = <<Index:32/integer, Begin:32/integer, Length:32/integer>>,

         meck:sequence(network_utils, socket_get, 2, [<<6>>, MockPayload]),
         {Type, Message} = receive_message(mock_socket, 13),
         ?assertEqual(Type, request),
         ?assertEqual(Message, #{index => 1, piece_begin => 2, length => 123})
     end},
    {"Type 'piece' is parsed correctly",
     fun() ->
         Index = 1,
         Begin = 2,
         Block = <<9,8,9,8>>,
         MockPayload = <<Index:32/integer, Begin:32/integer, Block/binary>>,

         meck:sequence(network_utils, socket_get, 2, [<<7>>, MockPayload]),
         {Type, Message} = receive_message(mock_socket, 13),
         ?assertEqual(Type, piece),
         ?assertEqual(Message, #{index => 1, piece_begin => 2, binary => <<9,8,9,8>>})
     end},
    {"Type 'cancel' is parsed correctly",
     fun() ->
         Index = 1,
         Begin = 2,
         Length = 123,
         MockPayload = <<Index:32/integer, Begin:32/integer, Length:32/integer>>,

         meck:sequence(network_utils, socket_get, 2, [<<8>>, MockPayload]),
         {Type, Message} = receive_message(mock_socket, 13),
         ?assertEqual(Type, cancel),
         ?assertEqual(Message, #{index => 1, piece_begin => 2, length => 123})
     end}]}.
