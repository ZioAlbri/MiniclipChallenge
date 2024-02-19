-module(message_manager).
-export([init/0, create_message/2]).


init() ->
    case ets:info(messages) of
        undefined ->  % Check if the table does not exist
            ets:new(messages, [named_table, set, public]);
        _ ->
            ok
    end,
    StoredMessages = message_db:init(),
    lists:foreach(
        fun(StoredMessage) ->
            io:format("Found stored message in db: ~p~n", [StoredMessage]),
            Id = message:get_id(StoredMessage),
            ets:insert(messages, {Id, StoredMessage})
        end,
        StoredMessages
    ).


create_message(RoomId, Text) ->
    Id = collection_utils:get_highest_id(messages) + 1,
    Message = message:new(Id, RoomId, Text),
    ets:insert(messages, {Id, Message}),
    message_db:create_message(Message),
    Message.