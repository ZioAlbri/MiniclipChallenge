-module(room_db).
-export([init/0, get_rooms/0, create_room/1, update_room/1, delete_room/1]).

init() ->
    db:create_table("Room"),
    StoredRooms = get_rooms(),
    StoredRooms.



get_rooms() ->
    FileName = "all_rooms.json",
    db:get_all_items("Room", FileName),
    {ok, Data} = file:read_file(FileName),
    StoredRooms = [room:transform_jsonitem_to_room(Room) || Room <- maps:get(<<"Items">>, jsx:decode(Data))],
    file:delete(FileName),
    StoredRooms.

create_room(Room) ->
    FileName = "room" ++ integer_to_list(room:get_id(Room)) ++ ".json",
    Data = room_to_json(Room),
    io:format("RoomData: ~p~n", [Data]),
    file:write_file(FileName, Data),
    db:create_item("Room", FileName),
    file:delete(FileName),
    ok.

update_room(Room) ->
    delete_room(Room),
    create_room(Room),
    ok.

delete_room(Room) ->
    KeyFileName = "room_key_" ++ integer_to_list(room:get_id(Room)) ++ ".json",
    KeyData = "{\"id\": {\"N\": \"" ++ integer_to_list(room:get_id(Room)) ++ "\"}}",
    file:write_file(KeyFileName, KeyData),
    db:delete_item("Room", KeyFileName),
    file:delete(KeyFileName),
    ok.




room_to_json(Room) ->
    JsonData = "{ " 
        "\"id\": {\"N\": \"" ++ integer_to_list(room:get_id(Room)) ++ "\"}, " 
        "\"owner\": {\"M\": " ++ user_db:user_to_json(room:get_owner(Room)) ++ "}, " 
        "\"members\": {\"L\": [" ++ user_db:users_to_json(room:get_members(Room)) ++ "]}, " 
        "\"is_private\": {\"BOOL\": " ++ atom_to_list(room:get_accessibility(Room)) ++ "}, " 
        "\"invited\": {\"L\": [" ++ user_db:users_to_json(room:get_invited(Room)) ++ "]} " 
    "}",
    JsonData.









