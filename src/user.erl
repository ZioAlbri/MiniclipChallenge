-module(user).
-export([new/2, get_id/1, get_name/1, get_socket/1]).

new(Name, Socket) ->
    {user, erlang:unique_integer([positive, monotonic]), {Name, Socket}}.

get_id({user, Id, {_, _}}) ->
    Id.

get_name({user, _, {Name, _}}) ->
    Name.

get_socket({user, _, {_, Socket}}) ->
    Socket.