-module(server).
-export([start/0, handle_connection/1]).

-define(COMMANDS, "What do you want to do? Type: 
        c -> Create a new room;
        l -> List all rooms;").

start() ->
    {ok, ListenSocket} = gen_tcp:listen(12345, [binary, {active, false}]),
    io:format("Server listening on port 12345... ~n~n"),
    room_manager:init(),
    io:format("Rooms manager initialized... ~n~n"),
    accept_connections(ListenSocket).

accept_connections(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    %% spawn to create a new lightweight process for the specific connection
    spawn(fun() -> handle_connection(Socket) end), 
    accept_connections(ListenSocket).

handle_connection(Socket) ->
    case get_client_name(Socket) of
        {ok, Data} ->
            %% Process the received client name
            ClientName = string:trim(binary_to_list(Data)),
            io:format("Connected: ~s~n", [ClientName]),
            %% Keep handling data from the connected client
            handle_data(Socket, ClientName);
        {error, Reason} ->
            %% Handle errors
            io:format("Connection error: ~p~n", [Reason]),
            ok %% Terminate the handling process
    end.

get_client_name(Socket) ->
    %% Prompt the client for their name
    send_string(Socket, "Enter your name: "),
    %% Receive the client's name
    gen_tcp:recv(Socket, 0).

handle_data(Socket, ClientName) ->
    send_string(Socket, ?COMMANDS),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            manage_command(Socket, Data, ClientName),
            %% Keep handling data from the connected client
            handle_data(Socket, ClientName);
        {error, closed} ->
            %% Connection closed by the client
            io:format("Connection closed by ~s~n", [ClientName]),
            ok; %% Terminate the handling process
        {error, Reason} ->
            %% Handle errors
            io:format("Error for ~s: ~p~n", [ClientName, Reason]),
            ok %% Terminate the handling process
    end.

manage_command(Socket, Data, ClientName) ->
    Command = string:trim(binary_to_list(Data)),
    io:format("Received command ~s from ~p~n", [Command, ClientName]),
    case Command of
        "c" -> 
            room_manager:create_room(ClientName),
            send_string(Socket, "Room created.");
        "l" -> 
            send_string(Socket, room_manager:get_rooms());
        _ ->
            send_string(Socket, "Wrong command. Please try again.")
    end.

send_string(Socket, Value) ->
    gen_tcp:send(Socket, Value).