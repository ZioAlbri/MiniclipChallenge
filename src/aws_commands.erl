-module(aws_commands).
-export([execute_command/1]).

execute_command(Command) ->
    Result = remove_newlines(Command),
    case os:cmd(Result) of
        Output ->
            io:format("Executed aws command"),
            io:format("Output: ~p~n", [Output]),
            string:tokens(Output, "\n");
        _ ->
            io:format("Aws command error")
    end.

remove_newlines(String) ->
   re:replace(String, "[\r\n]", " ", [global, {return, list}]).
