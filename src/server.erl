-module(server).
-export([start/0, handle_connection/1]).

-define(COMMANDS, "What do you want to do? Type: 
        c -> Create a new room;
        d -> Destroy a room you own;
        l -> List all rooms;
        j -> Join an existing room;
        q -> Leave a joined room;
        m -> Send message in a room;
        p -> Send private message.
        ").

start() ->
    {ok, ListenSocket} = gen_tcp:listen(12345, [binary, {active, false}]),
    io:format("Server listening on port 12345... ~n~n"),
    user_manager:init(),
    io:format("Users manager initialized... ~n~n"),
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
            %% Store user info for private messages
            user_manager:create_user(ClientName, Socket),
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
            manage_createroom_command(Socket, ClientName);
        "d" -> 
            manage_deleteroom_command(Socket, ClientName);
        "l" -> 
            manage_listrooms_command(Socket);
        "j" -> 
            manage_joinroom_command(Socket, ClientName);
        "q" -> 
            manage_leaveroom_command(Socket, ClientName);
        "m" -> 
            manage_sendmessage_command(Socket, ClientName);
        "p" -> 
            manage_sendprivatemessage_command(Socket, ClientName);
        _ ->
            send_string(Socket, "Wrong command. Please try again.")
    end. 

manage_createroom_command(Socket, ClientName) ->
    send_string(Socket, "Type the type of room you want to create (0 = public, 1 = private, <default> = public): "),
    {ok, RoomTypeData} = gen_tcp:recv(Socket, 0),
    try
        RoomType = list_to_integer(string:trim(binary_to_list(RoomTypeData))),
        case RoomType of
            1 ->
                room_manager:create_room(ClientName, Socket, true);
            _ ->
                room_manager:create_room(ClientName, Socket, false)
        end,       
        send_string(Socket, "Room created. ")
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer (0 = public, 1 = private) as room type. ")
    end.

manage_deleteroom_command(Socket, ClientName) ->
    send_string(Socket, "Type the id of the room you want to destroy: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        Result = room_manager:destroy_room(ClientName, RoomId),
        case Result of
            ok ->
                send_string(Socket, "Room destroyed. ");
            non_matching ->
                send_string(Socket, "You can't destroy rooms you don't own! ");
            not_found ->
                send_string(Socket, "Can't find the room. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as room id. ")
    end.

manage_listrooms_command(Socket) ->
    send_string(Socket, room_manager:get_rooms()).

manage_joinroom_command(Socket, ClientName) ->
    send_string(Socket, "Type the id of the room you want to join: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        Result = room_manager:join_room(ClientName, Socket, RoomId),
        case Result of
            ok ->
                send_string(Socket, "Room joined. ");
            not_found ->
                send_string(Socket, "Can't find the room. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as room id. ")
    end.

manage_leaveroom_command(Socket, ClientName) ->
    send_string(Socket, "Type the id of the room you want to leave: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        Result = room_manager:leave_room(ClientName, Socket, RoomId),
        case Result of
            ok ->
                send_string(Socket, "Room left. ");
            not_found ->
                send_string(Socket, "Can't find the room. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as room id. ")
    end.

manage_sendmessage_command(Socket, ClientName) ->
    send_string(Socket, "Type the id of the room: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        RoomSockets = room_manager:get_room_sockets(RoomId),
        case RoomSockets of
            {ok, Sockets} ->
                send_string(Socket, "Type the message you want to send: "),
                {ok, MessageData} = gen_tcp:recv(Socket, 0),
                Message = ClientName ++ " from room " ++ integer_to_list(RoomId) ++ " says: " ++binary_to_list(MessageData), % 
                send_strings(Sockets, Message),
                send_string(Socket, "Message sent. ");
            not_found ->
                send_string(Socket, "Can't find the room. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as room id. ")
    end.

manage_sendprivatemessage_command(Socket, ClientName) ->
    send_string(Socket, "Type the id of the user: "),
    {ok, DastinationUserIdData} = gen_tcp:recv(Socket, 0),
    try
        DestinationUserId = list_to_integer(string:trim(binary_to_list(DastinationUserIdData))),
        DestinationUser = user_manager:find_user_by_id(DestinationUserId),
        case DestinationUser of
            {ok, User} ->
                send_string(Socket, "Type the message you want to send: "),
                {ok, MessageData} = gen_tcp:recv(Socket, 0),
                Message = ClientName ++ " says (private message): " ++ binary_to_list(MessageData), % 
                send_string(user:get_socket(User), Message),
                send_string(Socket, "Message sent. ");
            not_found ->
                send_string(Socket, "Can't find the user. Try with another name. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as user id. ")
    end.



send_strings([], _) ->
    ok;  % Base case: empty list
send_strings([Socket | Rest], Message) ->
    send_string(Socket, Message),
    send_strings(Rest, Message).

send_string(Socket, Value) ->
    gen_tcp:send(Socket, Value).