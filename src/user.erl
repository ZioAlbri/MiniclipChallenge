-module(user).
-export([new/2, get_id/1, get_name/1, check_name/2, find_by_name/2, get_socket/1, set_socket/2, transform_jsonitem_to_user/1]).

-record(user, {id, name, socket = undefined}).

new(Name, Socket) ->
    #user{id = erlang:unique_integer([positive, monotonic]), name = Name, socket = Socket}.

get_id(User) ->
    User#user.id.

get_name(User) ->
    User#user.name.

check_name(User, UserName) ->
    User#user.name == UserName.

find_by_name(Name, Users) ->
    [{Id, User} || {Id, User} <- Users, User#user.name == Name].

get_socket(User) ->
    User#user.socket.

set_socket(User, Socket) ->
    User#user{socket = Socket}.

transform_jsonitem_to_user(Item) ->
    Id = list_to_integer(string:trim(binary_to_list(maps:get(<<"N">>, maps:get(<<"id">>, Item))))),
    Name = string:trim(binary_to_list(maps:get(<<"S">>, maps:get(<<"name">>, Item)))),
    #user{
      id = Id,
      name = Name
    }.