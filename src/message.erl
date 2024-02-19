-module(message).
-export([new/1, get_text/1, transform_jsonitem_to_message/1]).

%-record(message, {id, roomId, text}).
-record(message, {text}).

%new(Id, RoomId, Message) ->
new(Message) ->
    %#message{id = Id, roomId = RoomId, text = Message}.
    #message{text = Message}.

%get_id(Message) ->
    %Message#message.id.

%get_roomId(Message) ->
    %Message#message.roomId.

get_text(Message) ->
    Message#message.text.

transform_jsonitem_to_message(Item) ->
    Text = binary_to_list(maps:get(<<"S">>, Item)),
    #message{
      text = Text
    }.