-module(user_db).
-export([init/0, get_users/0, create_user/1, user_to_json/1, users_to_json/1]).

init() ->
    db:create_table("User"),
    StoredUsers = get_users(),
    StoredUsers.



get_users() ->
    FileName = "all_users.json",
    db:get_all_items("User", FileName),
    {ok, Data} = file:read_file(FileName),
    StoredUsers = [user:transform_jsonitem_to_user(User) || User <- maps:get(<<"Items">>, jsx:decode(Data))],
    file:delete(FileName),
    StoredUsers.

create_user(User) ->
    FileName = "user" ++ integer_to_list(user:get_id(User)) ++ ".json",
    Data = user_to_json(User),
    file:write_file(FileName, Data),
    db:create_item("User", FileName),
    file:delete(FileName),
    ok.



user_to_json(User) ->
    JsonData = "{ "
        "\"id\": {\"N\": \"" ++ integer_to_list(user:get_id(User)) ++ "\"}, "
        "\"name\": {\"S\": \"" ++ user:get_name(User) ++ "\"} "
    "}",
    JsonData.

users_to_json(Users) ->
    Result = lists:foldl(fun(User, Acc) ->
        Acc ++ "{\"M\": " ++ user_db:user_to_json(User) ++ "},"
    end, "", Users),
    lists:sublist(Result, length(Result) - 1). %to remove last comma