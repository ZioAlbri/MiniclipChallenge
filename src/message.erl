-module(message).
-export([new/3, get_id/1]).

-record(message, {id, roomId, text}).

new(Id, RoomId, Message) ->
    #message{id = Id, roomId = RoomId, text = Message}.

get_id(Message) ->
    Message#message.id.
