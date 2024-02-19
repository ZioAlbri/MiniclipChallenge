-module(message_db).
-export([init/0, get_messages/1, create_message/2, message_to_json/1]).

init() ->
    db:create_table("Message"),
    ok.

get_messages(RoomId) ->
    FileName = "all_messages_" + integer_to_list(RoomId) + ".json",
    db:get_all_items("Message", FileName),
    {ok, Data} = file:read_file(FileName),
    StoredMessages = [user:transform_jsonitem_to_user(User) || User <- maps:get(<<"Items">>, jsx:decode(Data))],
    file:delete(FileName),
    StoredMessages.

create_message(RoomId, Message) ->
    OperationId = integer_to_list(erlang:unique_integer([positive, monotonic])),

    KeyData = "{\"id\": {\"N\": \"" ++ integer_to_list(RoomId) ++ "\"}}",
    KeyDataFileName = "message_key_" ++ integer_to_list(RoomId) ++ "_" ++ OperationId ++ ".json",
    file:write_file(KeyDataFileName, KeyData),

    UpdateExp = "SET #messages = list_append(if_not_exists(#messages, :empty_list), :new_message)",
    UpdateExpFileName = "message_update_" ++ integer_to_list(RoomId) ++ "_" ++ OperationId ++ ".json",
    file:write_file(UpdateExpFileName, UpdateExp),

    AttributeExpNames = "{\"#messages\": \"messages\"}",
    AttributeExpNamesFileName = "message_names_" ++ integer_to_list(RoomId) ++ "_" ++ OperationId ++ ".json",
    file:write_file(AttributeExpNamesFileName, AttributeExpNames),

    AttributeExpValues = "{\":empty_list\": {\"L\": []}, \":new_message\": {\"L\": [{\"M\": " ++ message_to_json(Message) ++ "}]}}",
    AttributeExpValuesFileName = "message_values_" ++ integer_to_list(RoomId) ++ "_" ++ OperationId ++ ".json",
    file:write_file(AttributeExpValuesFileName, AttributeExpValues),

    db:append_item("Message", KeyDataFileName, UpdateExpFileName, AttributeExpNamesFileName, AttributeExpValuesFileName),
    file:delete(KeyDataFileName),
    file:delete(UpdateExpFileName),
    file:delete(AttributeExpNamesFileName),
    file:delete(AttributeExpValuesFileName),
    ok.


message_to_json(Message) ->
    JsonData = "{\"message\": {\"S\": \"" ++ Message ++ "\"}}",
    JsonData.