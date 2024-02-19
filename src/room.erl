-module(room).
-export([new/3, 
    get_id/1, 
    get_owner/1, 
    get_owner_name/1, 
    get_members/1, 
    get_members_names/1, 
    get_members_sockets/1, 
    is_member/2,
    update_members/2,
    get_accessibility/1, 
    join/2, 
    leave/2, 
    get_invited/1, 
    invite/2, 
    is_invited/2,
    update_invited/2,
    transform_jsonitem_to_room/1]).

-record(room, {
    id,
    owner,
    members,
    is_private,
    invited
}).

new(Id, Owner, IsPrivate) ->
    #room{
        id = Id,
        owner = Owner,
        members = [Owner],
        is_private = IsPrivate,
        invited = [Owner]
    }.

get_id(Room) ->
    Room#room.id.

get_owner(Room) ->
    Room#room.owner.

get_owner_name(Room) ->
    user:get_name(Room#room.owner).

get_members(Room) ->
    Room#room.members.

get_members_names(Room) ->
    [user:get_name(User) || User <- Room#room.members].

get_members_sockets(Room) ->
    [user:get_socket(User) || User <- Room#room.members].

is_member(Room, User) ->
    lists:any(fun(U) -> user:check_id(U, user:get_id(User)) end, Room#room.members).

update_members(Room, UpdatedMembers) ->
    Room#room{members = UpdatedMembers}.

get_accessibility(Room) ->
    Room#room.is_private.

join(User, Room) ->
    UpdatedMembers = case lists:member(User, Room#room.members) of
        true ->
            Room#room.members;
        false ->
            [User | Room#room.members]
    end,
    Room#room{members = UpdatedMembers}.

leave(User, Room) ->
    UpdatedMembers = lists:delete(User, Room#room.members),
    Room#room{members = UpdatedMembers}.

get_invited(Room) ->
    Room#room.invited.

invite(User, Room) ->
    UpdatedInvited = case is_invited(Room, User) of
        true ->
            Room#room.invited;
        false ->
            [User | Room#room.invited]
    end,
    Room#room{invited = UpdatedInvited}.

is_invited(Room, User) ->
    lists:any(fun(U) -> user:check_id(U, user:get_id(User)) end, Room#room.invited).

update_invited(Room, UpdatedInvited) ->
    Room#room{invited = UpdatedInvited}.

transform_jsonitem_to_room(Item) ->
    Id = list_to_integer(string:trim(binary_to_list(maps:get(<<"N">>, maps:get(<<"id">>, Item))))),
    Owner = user:transform_jsonitem_to_user(maps:get(<<"M">>, maps:get(<<"owner">>, Item))),
    Members = maps:get(<<"L">>, maps:get(<<"members">>, Item)),
    IsPrivate = maps:get(<<"BOOL">>, maps:get(<<"is_private">>, Item)),
    Invited = maps:get(<<"L">>, maps:get(<<"invited">>, Item)),

    TransformedMembers = [user:transform_jsonitem_to_user(maps:get(<<"M">>, Member)) || Member <- Members],
    TransformedInvited = [user:transform_jsonitem_to_user(maps:get(<<"M">>, InvitedUser)) || InvitedUser <- Invited],

    #room{
        id = Id,
        owner = Owner,
        members = TransformedMembers,
        is_private = IsPrivate,
        invited = TransformedInvited
    }.