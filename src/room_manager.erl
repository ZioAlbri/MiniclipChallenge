-module(room_manager).
-export([init/0, create_room/2, destroy_room/2, join_room/2, leave_room/2, invite_user/3, get_room_sockets/1, update_sockets_for_user/1, get_rooms/1]).


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
    Room = room:new(Owner, IsPrivate),
    Id = room:get_id(Room),
    ets:insert(rooms, {Id, Room}),
    io:format("User ~p has created room ~p~n", [Owner, Room]),
    room_db:create_room(Room),
    ok.


destroy_room(Owner, RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            RoomOwner = room:get_owner(Room),
            Result = if
                RoomOwner == Owner ->
                    ets:delete(rooms, RoomId),
                    room_db:delete_room(Room), 
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
            if
                RoomOwner == Owner ->
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


get_room_sockets(RoomId) ->
    case find_room_by_id(RoomId) of
        {ok, Room} ->
            Sockets = room:get_members_sockets(Room),
            {ok, Sockets};
        not_found ->
            not_found
    end.


update_sockets_for_user(User) ->
    UserName = user:get_name(User),
    lists:foreach(
        fun({_, Room}) ->

            io:format("Room: ~p~n", [Room]),

            UpdatedMembers = lists:map(
                fun(RoomMember) ->
                    case user:check_name(RoomMember, UserName) of
                        true ->
                            io:format("Socket modified"),
                            user:set_socket(RoomMember, user:get_socket(User));
                        false ->
                            io:format("Socket NOT modified"),
                            RoomMember
                    end
                end,
                room:get_members(Room)
            ),

            io:format("Updated Members: ~p~n", [UpdatedMembers]),

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

            io:format("Updated Invited: ~p~n", [UpdatedInvited]),

            UpdatedRoom = room:update_members(Room, UpdatedMembers),
            UpdatedRoom2 = room:update_invited(UpdatedRoom, UpdatedInvited),
            io:format("UpdatedRoom: ~p~n", [UpdatedRoom2]),
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

