-module(message_manager).
-export([init/0, create_message/2, get_messages/2]).


init() ->
    message_db:init(),
    ok.

create_message(RoomId, Text) ->
    Message = message:new(lists:filter(fun(X) -> X /= $\r andalso X /= $\n end, Text)),
    message_db:create_message(RoomId, Message),
    Message.

get_messages(Room, User) ->
    RoomId = room:get_id(Room),
    case room:is_member(Room, User) of
        true ->
            {ok, message_db:get_messages(RoomId)};
        false ->
            error
    end.