-module(user_manager).
-export([init/0, create_user/2, get_users_sockets/0, find_user_by_id/1]).


init() ->
    case ets:info(users) of
        undefined ->  % Check if the table does not exist
            ets:new(users, [named_table, set, public]);
        _ ->
            ok
    end.


create_user(Name, Socket) ->
    User = user:new(Name, Socket),
    Id = user:get_id(User),
    ets:insert(users, {Id, User}),
    io:format("User ~s has been created with id ~p~n", [Name, Id]),
    ok.


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