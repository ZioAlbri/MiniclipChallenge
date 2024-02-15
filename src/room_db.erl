-module(room_db).
-export([init/0, put_room/1]).

init() ->

    CreateTableCommand = "aws dynamodb create-table --table-name Room 
        --attribute-definitions 
            AttributeName=id,AttributeType=N
        --key-schema 
            AttributeName=id,KeyType=HASH 
        --provisioned-throughput 
            ReadCapacityUnits=5,WriteCapacityUnits=5
        --endpoint http://localhost:8000",

    aws_commands:execute_command(CreateTableCommand),
    ok.

put_room(Room) ->
    RoomIdString = integer_to_list(room:get_id(Room)),
    JsonFileName = "room" ++ RoomIdString ++ ".json",
    JsonData = "{ " 
            "\"id\": {\"N\": \"" ++ RoomIdString ++ "\"}, " 
            "\"owner\": {\"S\": \"" ++ room:get_owner_name(Room) ++ "\"}, " 
            "\"members\": {\"L\": [" ++ create_json(room:get_members(Room)) ++ "]}, " 
            "\"is_private\": {\"BOOL\": " ++ atom_to_list(room:get_accessibility(Room)) ++ "}, " 
            "\"invited\": {\"L\": [" ++ create_json(room:get_invited(Room)) ++ "]} " 
        "}",
    file:write_file(JsonFileName, JsonData),

    PutRoomCommand = "aws dynamodb put-item " 
        "--table-name Room " 
        "--item file://" ++ JsonFileName ++ " "
        "--endpoint http://localhost:8000",
    PutRoomResult = aws_commands:execute_command(PutRoomCommand),
    io:format("PutRoom result: ~n~s~n~n", [PutRoomResult]),
    PutRoomResult.

create_item_json(User) ->
    "{\"M\": {\"id\": {\"N\": \"" ++ integer_to_list(user:get_id(User)) ++ "\"}, \"name\": {\"S\": \"" ++ user:get_name(User) ++ "\"}}},".
    
create_json(Users) ->
    Result = lists:foldl(fun(User, Acc) ->
        Acc ++ create_item_json(User)
    end, "", Users),
    lists:sublist(Result, length(Result) - 1). %remove last comma

