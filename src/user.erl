-module(user).
-export([new/2, get_id/1, get_name/1, get_socket/1, transform_jsonitem_to_user/1]).

-record(user, {id, name, socket}).

new(Name, Socket) ->
    #user{id = erlang:unique_integer([positive, monotonic]), name = Name, socket = Socket}.

get_id(User) ->
    User#user.id.

get_name(User) ->
    User#user.name.

get_socket(User) ->
    User#user.socket.

transform_jsonitem_to_user(Item) ->
    User = maps:get(<<"M">>, Item),
    Id = list_to_integer(string:trim(binary_to_list(maps:get(<<"N">>, maps:get(<<"id">>, User))))),
    Name = string:trim(binary_to_list(maps:get(<<"S">>, maps:get(<<"name">>, User)))),

    #user{
      id = Id,
      name = Name  
    }.