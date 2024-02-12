-module(room_manager).
-export([init/0, create_room/2, destroy_room/2, join_room/2, leave_room/2, invite_user/3, get_room_sockets/1, get_rooms/0]).


init() ->
    case ets:info(rooms) of
        undefined ->  % Check if the table does not exist
            ets:new(rooms, [named_table, set, public]);
        _ ->
            ok
    end.


create_room(Owner, IsPrivate) ->
    Room = room:new(Owner, IsPrivate),
    Id = room:get_id(Room),
    ets:insert(rooms, {Id, Room}),
    io:format("User ~p has created room ~p~n", [Owner, Room]),
    ok.


destroy_room(Owner, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            RoomOwner = room:get_owner(Room),
            Result = if
                RoomOwner == Owner ->
                    ets:delete(rooms, RoomId), 
                    io:format("User ~p has destroyed room ~p~n", [Owner, Room]),
                    ok;
                true ->
                    % Do nothing or perform a different action for non-matching owner
                    io:format("Owner ~p does not match expected owner ~p. No action performed.~n", [Owner, RoomOwner]),
                    non_matching
            end,
            Result;
        not_found ->
            io:format("Room not found.~n"),
            not_found        
    end.


join_room(User, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            UpdatedRoom = room:join(User, Room),
            ets:insert(rooms, {RoomId, UpdatedRoom}),
            io:format("User ~p has joined room ~p~n", [User, Room]),
            ok;
        not_found ->
            not_found
    end.


leave_room(User, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            UpdatedRoom = room:leave(User, Room),
            ets:insert(rooms, {RoomId, UpdatedRoom}),
            io:format("User ~p has left room ~p~n", [User, Room]),
            ok;
        not_found ->
            not_found
    end.


invite_user(Owner, User, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            RoomOwner = room:get_owner(Room),
            if
                RoomOwner == Owner ->
                    room:invite(User, Room), 
                    io:format("User ~p has been invited to room ~p~n", [User, Room]),
                    ok;
                true ->
                    % Do nothing or perform a different action for non-matching owner
                    io:format("Owner ~p does not match expected owner ~p. No action performed.~n", [Owner, RoomOwner]),
                    non_matching
            end;
        not_found ->
            not_found
    end.


get_room_sockets(RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            Sockets = room:get_members_sockets(Room),
            {ok, Sockets};
        not_found ->
            not_found
    end.



get_rooms() ->
    print_rooms(ets:tab2list(rooms)).

print_rooms(Rooms) ->
    print_rooms(Rooms, []).
print_rooms([], Accumulated) ->
    lists:reverse(Accumulated);  % Reverse the accumulated list to maintain the correct order
print_rooms([{Id, Room} | Rest], Accumulated) ->
    IsPrivate = room:get_accessibility(Room),
    RoomInfo = io_lib:format("Room ID: ~p~nOwner: ~p~nMembers: ~p~nPrivate: ~p~n~n", [Id, room:get_owner(Room), room:get_members_names(Room), IsPrivate]),
    case IsPrivate of
        true ->
                print_rooms(Rest, [Accumulated]);
        false ->
                print_rooms(Rest, [RoomInfo | Accumulated])
    end.


find_room_by_id(RoomId) ->
    case ets:lookup(rooms, RoomId) of
        [{RoomId, Room}] ->
            {ok, Room};
        [] ->
            not_found
    end.

