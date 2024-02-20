-module(aws_commands).
-export([execute_command/1]).

execute_command(Command) ->
    try
        Result = remove_newlines(Command),
        os:cmd(Result),
        ok
    catch
        error:Reason ->
            Reason;
        _ -> 
            ok
    end.

remove_newlines(String) ->
   re:replace(String, "[\r\n]", " ", [global, {return, list}]).
