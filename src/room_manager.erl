-module(room_manager).
-export([new/0, create_room/1, join_room/2, get_rooms/0, init/0]).

new() ->
    {room_manager, dict:new()}.

create_room(Owner) ->
    Room = room:new(Owner),
    Id = room:get_id(Room),
    UpdatedRooms = dict:store(Id, Room, get_rooms()),
    io:format("User ~s has created room ~p~n", [Owner, Room]),
    {ok, UpdatedRooms}.

join_room(User, RoomId) ->
    case dict:find(RoomId, get_rooms()) of
        {ok, Room} ->
            UpdatedRoom = room:join(User, Room),
            UpdatedRooms = dict:store(RoomId, UpdatedRoom, get_rooms()),
            io:format("User ~s has joined room ~p~n", [User, Room]),
            {ok, UpdatedRooms};
        error ->
            {error, not_found}
    end.

get_rooms() ->
    ets:tab2list(rooms).

init() ->
    ets:new(rooms, [named_table, set, public]).