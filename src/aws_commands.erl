-module(aws_commands).
-export([execute_command/1]).

execute_command(Command) ->
    Result = remove_newlines(Command),
    case os:cmd(Result) of
        Output ->
            io:format("~nCommand response: ~n~p~n~n", [Output]),
            string:tokens(Output, "\n")
    end.

remove_newlines(String) ->
   re:replace(String, "[\r\n]", " ", [global, {return, list}]).
