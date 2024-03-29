-module(room_manager).
-export([init/0, create_room/2, destroy_room/2, join_room/2, leave_room/2, invite_user/3, get_room_sockets/2, update_sockets_for_user/1, get_rooms/1, find_room_by_id/1]).


init() ->
    case ets:info(rooms) of
        undefined ->  % Check if the table does not exist
            ets:new(rooms, [named_table, set, public]);
        _ ->
            ok
    end,
    StoredRooms = room_db:init(),
    lists:foreach(
        fun(StoredRoom) ->
            io:format("Found stored room in db: ~p~n", [StoredRoom]),
            Id = room:get_id(StoredRoom),
            ets:insert(rooms, {Id, StoredRoom})
        end,
        StoredRooms
    ).


create_room(Owner, IsPrivate) ->
    Id = collection_utils:get_highest_id(rooms) + 1,
    Room = room:new(Id, Owner, IsPrivate),
    ets:insert(rooms, {Id, Room}),
    io:format("User ~p has created room ~p~n", [Owner, Room]),
    room_db:create_room(Room).



destroy_room(User, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            RoomOwner = room:get_owner(Room),
            OwnerId = user:get_id(RoomOwner),
            UserId = user:get_id(User),
            Result = if
                OwnerId == UserId ->
                    ets:delete(rooms, RoomId),
                    room_db:delete_room(Room), 
                    io:format("User ~p has destroyed room ~p~n", [User, Room]),
                    ok;
                true ->
                    % Do nothing or perform a different action for non-matching owner
                    io:format("User ~p does not match expected owner ~p. No action performed.~n", [User, RoomOwner]),
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
            IsPrivate = room:get_accessibility(Room),
            case IsPrivate of
                true ->
                    case room:is_invited(Room, User) of
                        true ->
                            UpdatedRoom = room:join(User, Room);
                        false ->
                            UpdatedRoom = Room
                    end;
                false ->
                    UpdatedRoom = room:join(User, Room)
            end,
            case UpdatedRoom == Room of
                true ->
                    not_found;
                false ->
                    ets:insert(rooms, {RoomId, UpdatedRoom}),
                    room_db:update_room(UpdatedRoom),
                    io:format("User ~p has joined room ~p~n", [User, UpdatedRoom]),
                    ok
            end;
        not_found ->
            not_found
    end.


leave_room(User, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            UpdatedRoom = room:leave(User, Room),
            ets:insert(rooms, {RoomId, UpdatedRoom}),
            room_db:update_room(UpdatedRoom),
            io:format("User ~p has left room ~p~n", [User, UpdatedRoom]),
            ok;
        not_found ->
            not_found
    end.


invite_user(Owner, User, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            RoomOwner = room:get_owner(Room),
            OwnerId = user:get_id(RoomOwner),
            UserId = user:get_id(Owner),
            if
                OwnerId == UserId ->
                    UpdatedRoom = room:invite(User, Room), 
                    ets:insert(rooms, {RoomId, UpdatedRoom}),
                    room_db:update_room(UpdatedRoom),
                    io:format("User ~p has been invited to room ~p~n", [User, UpdatedRoom]),
                    ok;
                true ->
                    % Do nothing or perform a different action for non-matching owner
                    io:format("Owner ~p does not match expected owner ~p. No action performed.~n", [Owner, RoomOwner]),
                    non_matching
            end;
        not_found ->
            not_found
    end.


get_room_sockets(RoomId, User) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            case room:is_member(Room, User) of
                true ->
                    Sockets = room:get_members_sockets(Room),
                    {ok, Sockets};
                false ->
                    error
            end;
        not_found ->
            not_found
    end.


update_sockets_for_user(User) ->
    UserName = user:get_name(User),
    lists:foreach(
        fun({_, Room}) ->

            UpdatedMembers = lists:map(
                fun(RoomMember) ->
                    case user:check_name(RoomMember, UserName) of
                        true ->
                            user:set_socket(RoomMember, user:get_socket(User));
                        false ->
                            RoomMember
                    end
                end,
                room:get_members(Room)
            ),

            UpdatedInvited = lists:map(
                fun(RoomInvited) ->
                    case user:check_name(RoomInvited, UserName) of
                        true ->
                            user:set_socket(RoomInvited, user:get_socket(User));
                        false ->
                            RoomInvited
                    end
                end,
                room:get_invited(Room)
            ),

            UpdatedRoom = room:update_members(Room, UpdatedMembers),
            UpdatedRoom2 = room:update_invited(UpdatedRoom, UpdatedInvited),
            ets:insert(rooms, {room:get_id(UpdatedRoom2), UpdatedRoom2})
        end,
        ets:tab2list(rooms)
    ).


get_rooms(User) ->
    print_rooms(ets:tab2list(rooms), User).

print_rooms(Rooms, User) ->
    print_rooms(Rooms, [], User).
print_rooms([], Accumulated, _) ->
    lists:reverse(Accumulated);  % Reverse the accumulated list to maintain the correct order
print_rooms([{Id, Room} | Rest], Accumulated, User) ->
    IsPrivate = room:get_accessibility(Room),
    RoomInfo = io_lib:format("Room ID: ~p~nOwner: ~p~nMembers: ~p~nPrivate: ~p~n~n", [Id, room:get_owner(Room), room:get_members_names(Room), IsPrivate]),
    case IsPrivate of
        true ->
            case room:is_invited(Room, User) of
                true ->
                    print_rooms(Rest, [RoomInfo | Accumulated], User);
                false ->
                    print_rooms(Rest, [Accumulated], User)
            end;
        false ->
                print_rooms(Rest, [RoomInfo | Accumulated], User)
    end.


find_room_by_id(RoomId) ->
    case ets:lookup(rooms, RoomId) of
        [{RoomId, Room}] ->
            {ok, Room};
        [] ->
            not_found
    end.

