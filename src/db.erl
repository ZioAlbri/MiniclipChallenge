-module(db).
-export([create_table/1, get_all_items/2, get_item/3, create_item/2, delete_item/2, append_item/5]).

-define(ENDPOINT, "http://localhost:8000").

create_table(TableName) ->
    try
        CreateTableCommand = "aws dynamodb create-table --table-name " ++ TableName ++ " 
            --attribute-definitions 
                AttributeName=id,AttributeType=N
            --key-schema 
                AttributeName=id,KeyType=HASH 
            --provisioned-throughput 
                ReadCapacityUnits=5,WriteCapacityUnits=5
            --endpoint " ++ ?ENDPOINT,

        aws_commands:execute_command(CreateTableCommand),
        ok
    catch
        error:Reason ->
            Reason;
        _ -> 
            ok
    end.


get_all_items(TableName, OutputJsonFile) ->
    GetAllItemsCommand = "aws dynamodb scan --table-name " ++ TableName ++ " --output json > " ++ OutputJsonFile ++ " --endpoint " ++ ?ENDPOINT,
    aws_commands:execute_command(GetAllItemsCommand),
    ok.

get_item(TableName, KeyJsonFile, OutputJsonFile) ->
    GetItemCommand = "aws dynamodb get-item --table-name " ++ TableName ++ " --key file://" ++ KeyJsonFile ++ " --output json > " ++ OutputJsonFile ++ " --endpoint " ++ ?ENDPOINT,
    aws_commands:execute_command(GetItemCommand),
    ok.

create_item(TableName, InputJsonFile) ->
    CreateRoomCommand = "aws dynamodb put-item " 
        "--table-name " ++ TableName ++ " " 
        "--item file://" ++ InputJsonFile ++ " "
        "--endpoint " ++ ?ENDPOINT,
    aws_commands:execute_command(CreateRoomCommand),
    ok.


delete_item(TableName, InputJsonFile) ->
    DeleteRoomCommand = "aws dynamodb delete-item " 
        "--table-name " ++ TableName ++ " " 
        "--key file://" ++ InputJsonFile ++ " "
        "--endpoint " ++ ?ENDPOINT,
    aws_commands:execute_command(DeleteRoomCommand),
    ok.

append_item(TableName, KeyJsonFile, UpdateExpJsonFile, AttributeExpNamesJsonFile, AttributeExpValJsonFile) ->
    AppendItemCommand = "aws dynamodb update-item " 
        "--table-name " ++ TableName ++ " " 
        "--key file://" ++ KeyJsonFile ++ " "
        "--update-expression file://" ++ UpdateExpJsonFile ++ " "
        "--expression-attribute-names file://" ++ AttributeExpNamesJsonFile ++ " "
        "--expression-attribute-values file://" ++ AttributeExpValJsonFile ++ " "
        "--endpoint " ++ ?ENDPOINT,
    aws_commands:execute_command(AppendItemCommand),
    ok.