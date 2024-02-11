-module(room_manager).
-export([init/0, create_room/1, join_room/2, get_rooms/0,  print_rooms/1]).

init() ->
    case ets:info(rooms) of
        undefined ->  % Check if the table does not exist
            ets:new(rooms, [named_table, set, public]);
        _ ->
            ok
    end.

create_room(Owner) ->
    Room = room:new(Owner),
    Id = room:get_id(Room),
    io:format("Inserting room with Id ~p~n", [Id]),
    ets:insert(rooms, {Id, Room}),
    io:format("User ~s has created room ~p~n", [Owner, Room]),
    ok.

join_room(User, RoomId) ->
    case dict:find(RoomId, get_rooms()) of
        {ok, Room} ->
            UpdatedRoom = room:join(User, Room),
            dict:store(RoomId, UpdatedRoom, get_rooms()),
            io:format("User ~s has joined room ~p~n", [User, Room]),
            ok;
        error ->
            {error, not_found}
    end.

get_rooms() ->
    ets:tab2list(rooms).

print_rooms([]) ->
    ok;  % base case, empty list
print_rooms([{Id, _Room} | Rest]) ->
    io:format("Room ID: ~p~n", [Id]),
    io:format("Owner: ~p~n~n", [room:get_owner(_Room)]),
    print_rooms(Rest).