-module(server).
-export([start/0, handle_connection/1]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(12345, [binary, {active, false}]),
    io:format("Server listening on port 12345... ~n~n"),
    accept_connections(ListenSocket).

accept_connections(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    %% spawn to create a new lightweight process for the specific connection
    spawn(fun() -> handle_connection(Socket) end), 
    accept_connections(ListenSocket).

handle_connection(Socket) ->
    case get_client_name(Socket) of
        {ok, ClientName} ->
            %% Process the received client name
            io:format("Connected: ~s~n", [ClientName]),
            gen_tcp:send(Socket, "What do you want to do? Type: "),
            gen_tcp:send(Socket, "c -> Create a new room;"),
            %% Keep handling data from the connected client
            handle_data(Socket, ClientName);
        {error, Reason} ->
            %% Handle errors
            io:format("Connection error: ~p~n", [Reason]),
            ok %% Terminate the handling process
    end.

get_client_name(Socket) ->
    %% Prompt the client for their name
    gen_tcp:send(Socket, "Enter your name: "),
    
    %% Receive the client's name
    gen_tcp:recv(Socket, 0).

handle_data(Socket, ClientName) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            manage_command(Socket, Data, ClientName);
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
    io:format("Received command ~s from ~p", [Command, ClientName]),
    case Command of
        "c" -> 
            {ok, UpdatedRoomManager} = room_manager:create_room(ClientName),
            gen_tcp:send(Socket, "Room created.");
            %%room_manager:loop(UpdatedRoomManager);
        _ ->
            gen_tcp:send(Socket, "Wrong command. Please try again.")
    end.