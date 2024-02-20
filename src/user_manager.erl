-module(user_manager).
-export([init/0, create_user/2, update_user/2, get_users_sockets/0, find_user_by_id/1, find_user_by_name/1]).


init() ->
    case ets:info(users) of
        undefined ->  % Check if the table does not exist
            ets:new(users, [named_table, set, public]);
        _ ->
            ok
    end,
    StoredUsers = user_db:init(),
    lists:foreach(
        fun(StoredUser) ->
            io:format("Found stored user in db: ~p~n", [StoredUser]),
            Id = user:get_id(StoredUser),
            ets:insert(users, {Id, StoredUser})
        end,
        StoredUsers
    ).


create_user(Name, Socket) ->
    Id = collection_utils:get_highest_id(users) + 1,
    User = user:new(Id, Name, Socket),
    ets:insert(users, {Id, User}),
    io:format("User ~s has been created with id ~p~n", [Name, Id]),
    user_db:create_user(User),
    User.

update_user(User, Socket) ->
    UpdatedUser = user:set_socket(User, Socket),
    Id = user:get_id(UpdatedUser),
    ets:insert(users, {Id, UpdatedUser}),
    io:format("User updated: ~p~n", [UpdatedUser]),
    UpdatedUser.


get_users_sockets() ->
    get_users_sockets(ets:tab2list(users)).

get_users_sockets(Users) ->
    get_users_sockets(Users, []).
get_users_sockets([], Accumulated) ->
    lists:reverse(Accumulated);  % Reverse the accumulated list to maintain the correct order
get_users_sockets([{_, User} | Rest], Accumulated) ->
    get_users_sockets(Rest, [user:get_socket(User) | Accumulated]).


find_user_by_id(UserId) ->
    case ets:lookup(users, UserId) of
        [{UserId, User}] ->
            {ok, User};
        [] ->
            not_found
    end.

find_user_by_name(UserName) ->
    Result = user:find_by_name(UserName, ets:tab2list(users)),
    case Result of
        [{_, User}] ->
            {ok, User};
        [] ->
            not_found
    end.