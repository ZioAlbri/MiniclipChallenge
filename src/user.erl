-module(user).
-export([new/2, get_id/1, get_name/1, get_socket/1]).

-record(user, {id, name, socket}).

new(Name, Socket) ->
    #user{id = erlang:unique_integer([positive, monotonic]), name = Name, socket = Socket}.

get_id(User) ->
    User#user.id.

get_name(User) ->
    User#user.name.

get_socket(User) ->
    User#user.socket.