-module(message_db).
-export([init/0, get_messages/1, create_message/2, message_to_json/1]).

init() ->
    db:create_table("Message"),
    ok.

get_messages(RoomId) -> 
    OperationId = integer_to_list(erlang:unique_integer([positive, monotonic])),

    OutputFile = "all_messages_" ++ integer_to_list(RoomId) ++ "_" ++ OperationId ++ ".json",

    KeyData = "{\"id\": {\"N\": \"" ++ integer_to_list(RoomId) ++ "\"}}",
    KeyDataFileName = "message_key_" ++ integer_to_list(RoomId) ++ "_" ++ OperationId ++ ".json",
    file:write_file(KeyDataFileName, KeyData),

    db:get_item("Message", KeyDataFileName, OutputFile), 
    {ok, Data} = file:read_file(OutputFile),
    Messages = [message:get_text(message:transform_jsonitem_to_message(Message)) || Message <- maps:get(<<"L">>, maps:get(<<"messages">>, maps:get(<<"Item">>, jsx:decode(Data))))],
    file:delete(OutputFile),
    file:delete(KeyDataFileName),
    Messages.

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

    AttributeExpValues = "{\":empty_list\": {\"L\": []}, \":new_message\": {\"L\": [" ++ message_to_json(Message) ++ "]}}",
    AttributeExpValuesFileName = "message_values_" ++ integer_to_list(RoomId) ++ "_" ++ OperationId ++ ".json",
    file:write_file(AttributeExpValuesFileName, AttributeExpValues),

    db:append_item("Message", KeyDataFileName, UpdateExpFileName, AttributeExpNamesFileName, AttributeExpValuesFileName),
    file:delete(KeyDataFileName),
    file:delete(UpdateExpFileName),
    file:delete(AttributeExpNamesFileName),
    file:delete(AttributeExpValuesFileName),
    ok.


message_to_json(Message) ->
    %JsonData = "{\"N\": \"" ++ message:get_id(Message) ++ "\"}, {\"N\": \"" ++ message:get_roomId(Message) ++ "\"}, {\"S\": \"" ++ message:get_text(Message) ++ "\"}",
    JsonData = "{\"S\": \"" ++ message:get_text(Message) ++ " \"}",
    JsonData.