-module(server).
-export([start/0, handle_connection/1]).

-define(COMMANDS, "What do you want to do? Type: 
        c -> Create a new room;
        d -> Destroy a room you own;
        l -> List all rooms;
        j -> Join an existing room;
        q -> Leave a joined room;
        m -> Send message in a room;
        p -> Send private message;
        i -> Invite user to a room you own;
        lm - > List a room's previous messages.
        ").

start() ->
    {ok, ListenSocket} = gen_tcp:listen(12346, [binary, {active, false}]),
    io:format("Server listening on port 12345... ~n~n"),
    user_manager:init(),
    io:format("Users manager initialized... ~n~n"),
    room_manager:init(),
    io:format("Rooms manager initialized... ~n~n"),
    message_manager:init(),
    io:format("Messages manager initialized... ~n~n"),
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
            User = login(ClientName, Socket),
            %% Keep handling data from the connected client
            handle_data(User);
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

login(ClientName, Socket) ->
    case user_manager:find_user_by_name(ClientName) of
        {ok, U} ->
            User = user_manager:update_user(U, Socket),
            room_manager:update_sockets_for_user(User),
            send_string(Socket, "Welcome back!");
        not_found ->
            User = user_manager:create_user(ClientName, Socket)
    end,
    User.

handle_data(User) ->
    send_string(user:get_socket(User), ?COMMANDS),
    case gen_tcp:recv(user:get_socket(User), 0) of
        {ok, Data} ->
            manage_command(Data, User),
            %% Keep handling data from the connected client
            handle_data(User);
        {error, closed} ->
            %% Connection closed by the client
            io:format("Connection closed by ~s~n", [user:get_name(User)]),
            ok; %% Terminate the handling process
        {error, Reason} ->
            %% Handle errors
            io:format("Error for ~s: ~p~n", [user:get_name(User), Reason]),
            ok %% Terminate the handling process
    end.



manage_command(Data, User) ->
    Command = string:trim(binary_to_list(Data)),
    io:format("Received command ~s from ~p~n", [Command, user:get_name(User)]),
    case Command of
        "c" -> 
            manage_createroom_command(User);
        "d" -> 
            manage_deleteroom_command(User);
        "l" -> 
            manage_listrooms_command(User);
        "j" -> 
            manage_joinroom_command(User);
        "q" -> 
            manage_leaveroom_command(User);
        "m" -> 
            manage_sendmessage_command(User);
        "p" -> 
            manage_sendprivatemessage_command(User);
        "i" -> 
            manage_inviteuser_command(User);
        "lm" -> 
            manage_listmessages_command(User);
        _ ->
            send_string(user:get_socket(User), "Wrong command. Please try again.")
    end. 

manage_createroom_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the type of room you want to create (0 = public, 1 = private, <default> = public): "),
    {ok, RoomTypeData} = gen_tcp:recv(Socket, 0),
    try
        RoomType = list_to_integer(string:trim(binary_to_list(RoomTypeData))),
        case RoomType of
            1 ->
                room_manager:create_room(User, true);
            _ ->
                room_manager:create_room(User, false)
        end,       
        send_string(Socket, "Room created. ")
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer (0 = public, 1 = private) as room type. ")
    end.

manage_deleteroom_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the id of the room you want to destroy: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        Result = room_manager:destroy_room(User, RoomId),
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

manage_listrooms_command(User) ->
    send_string(user:get_socket(User), room_manager:get_rooms(User)).

manage_joinroom_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the id of the room you want to join: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        Result = room_manager:join_room(User, RoomId),
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

manage_leaveroom_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the id of the room you want to leave: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        Result = room_manager:leave_room(User, RoomId),
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

manage_sendmessage_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the id of the room: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        RoomSockets = room_manager:get_room_sockets(RoomId, User),
        case RoomSockets of
            {ok, Sockets} ->
                send_string(Socket, "Type the message you want to send: "),
                {ok, MessageData} = gen_tcp:recv(Socket, 0),
                Message = user:get_name(User) ++ " from room " ++ integer_to_list(RoomId) ++ " says: " ++binary_to_list(MessageData), 
                send_strings(Sockets, Message),
                send_string(Socket, "Message sent. "),
                message_manager:create_message(RoomId, Message);
            error ->
                send_string(Socket, "You can't send messages in rooms you are not member. ");
            not_found ->
                send_string(Socket, "Can't find the room. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as room id. ")
    end.

manage_sendprivatemessage_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the id of the user: "),
    {ok, DastinationUserIdData} = gen_tcp:recv(Socket, 0),
    try
        DestinationUserId = list_to_integer(string:trim(binary_to_list(DastinationUserIdData))),
        DestinationUserResult = user_manager:find_user_by_id(DestinationUserId),
        case DestinationUserResult of
            {ok, DestinationUser} ->
                send_string(Socket, "Type the message you want to send: "),
                {ok, MessageData} = gen_tcp:recv(Socket, 0),
                Message = user:get_name(User) ++ " says (private message): " ++ binary_to_list(MessageData), % 
                send_string(user:get_socket(DestinationUser), Message),
                send_string(Socket, "Message sent. ");
            not_found ->
                send_string(Socket, "Can't find the user. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as user id. ")
    end.


manage_inviteuser_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the room id: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    send_string(Socket, "Type the user id: "),
    {ok, DastinationUserIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        DestinationUserId = list_to_integer(string:trim(binary_to_list(DastinationUserIdData))),
        DestinationUserResult = user_manager:find_user_by_id(DestinationUserId),
        case DestinationUserResult of
            {ok, DestinationUser} ->
                Result = room_manager:invite_user(User, DestinationUser, RoomId),
                case Result of
                    ok ->
                        send_string(Socket, "Invite sent. "),
                        send_string(user:get_socket(DestinationUser), "You've been invited to join room " ++ integer_to_list(RoomId));
                    non_matching ->
                        send_string(Socket, "You can't invite users to rooms you don't own! ");
                    not_found ->
                        send_string(Socket, "Can't find the room. Try with another id. ")
                end;
            not_found ->
                send_string(Socket, "Can't find the user. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as room id and user id. ")
    end.


manage_listmessages_command(User) ->
    Socket = user:get_socket(User),
    send_string(Socket, "Type the id of the room where you want to read messages: "),
    {ok, RoomIdData} = gen_tcp:recv(Socket, 0),
    try
        RoomId = list_to_integer(string:trim(binary_to_list(RoomIdData))),
        case message_manager:get_messages(RoomId, User) of
            Messages ->
                send_messages(Socket, ["Room Messages:" | Messages]);
            not_found ->
                send_string(Socket, "Can't find the room. Try with another id. ")
        end
    catch
        error:badarg ->
            send_string(Socket, "You have to insert an integer as room id. ")
    end.



send_strings([], _) ->
    ok;  % Base case: empty list
send_strings([Socket | Rest], Message) ->
    send_string(Socket, Message),
    send_strings(Rest, Message).

send_messages(_, []) ->
    ok;  % Base case: empty list
send_messages(Socket, [Message | Rest]) ->
    send_string(Socket, Message ++ "
        "),
    send_messages(Socket, Rest).

send_string(Socket, Value) ->
    gen_tcp:send(Socket, Value).