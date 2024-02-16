-module(room_db).
-export([init/0, get_rooms/0, create_room/1, update_room/1, delete_room/1]).

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
    StoredRooms = get_rooms(),
    StoredRooms.

get_rooms() ->
    GetAllRoomsCommand = "aws dynamodb scan --table-name Room --output json > all_rooms.json --endpoint http://localhost:8000",
    aws_commands:execute_command(GetAllRoomsCommand),
    StoredRooms = parse_json_file("all_rooms.json"),
    StoredRooms.

create_room(Room) ->
    RoomIdString = integer_to_list(room:get_id(Room)),
    FileName = "room" ++ RoomIdString ++ ".json",
    Data = room_to_json(Room),
    file:write_file(FileName, Data),

    CreateRoomCommand = "aws dynamodb put-item " 
        "--table-name Room " 
        "--item file://" ++ FileName ++ " "
        "--endpoint http://localhost:8000",
    aws_commands:execute_command(CreateRoomCommand),
    file:delete(FileName),
    ok.

update_room(Room) ->
    delete_room(Room),
    create_room(Room),
    ok.

delete_room(Room) ->
    RoomIdString = integer_to_list(room:get_id(Room)),

    KeyFileName = "room_key_" ++ RoomIdString ++ ".json",
    KeyData = key_to_json(Room),
    file:write_file(KeyFileName, KeyData),

    DeleteRoomCommand = "aws dynamodb delete-item " 
        "--table-name Room " 
        "--key file://" ++ KeyFileName ++ " "
        "--endpoint http://localhost:8000",
    aws_commands:execute_command(DeleteRoomCommand),
    file:delete(KeyFileName),
    ok.


key_to_json(Room) ->
    JsonData = "{\"id\": {\"N\": \"" ++ integer_to_list(room:get_id(Room)) ++ "\"}}",
    JsonData.

room_to_json(Room) ->
    JsonData = "{ " 
        "\"id\": {\"N\": \"" ++ integer_to_list(room:get_id(Room)) ++ "\"}, " 
        "\"owner\": " ++ user_to_json(room:get_owner(Room)) ++ " " 
        "\"members\": {\"L\": [" ++ users_to_json(room:get_members(Room)) ++ "]}, " 
        "\"is_private\": {\"BOOL\": " ++ atom_to_list(room:get_accessibility(Room)) ++ "}, " 
        "\"invited\": {\"L\": [" ++ users_to_json(room:get_invited(Room)) ++ "]} " 
    "}",
    JsonData.

user_to_json(User) ->
    "{\"M\": {\"id\": {\"N\": \"" ++ integer_to_list(user:get_id(User)) ++ "\"}, \"name\": {\"S\": \"" ++ user:get_name(User) ++ "\"}}},".
users_to_json(Users) ->
    Result = lists:foldl(fun(User, Acc) ->
        Acc ++ user_to_json(User)
    end, "", Users),
    lists:sublist(Result, length(Result) - 1). %to remove last comma


parse_json_file(FilePath) ->
    {ok, Data} = file:read_file(FilePath),
    transform_to_rooms(jsx:decode(Data)).

% Function to transform JSON items to room records
transform_to_rooms(Json) ->
    Rooms = maps:get(<<"Items">>, Json),
    TransformedRooms = [room:transform_jsonitem_to_room(Room) || Room <- Rooms],
    TransformedRooms.




