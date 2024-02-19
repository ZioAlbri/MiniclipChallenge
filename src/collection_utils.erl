-module(collection_utils).
-export([get_highest_id/1]).

get_highest_id(TableName) ->
    case ets:info(TableName, size) of
        0 -> 0; % handle empty collection
        _Size ->
            ets:foldl(fun({Id, _Room}, MaxId) ->
                            if Id > MaxId ->
                                   Id;
                               true ->
                                   MaxId
                            end
                      end, 0, TableName)
    end.