-module(db).
-export([create_table/1, get_all_items/2, create_item/2, delete_item/2, append_item/5]).

-define(ENDPOINT, "http://localhost:8000").

create_table(TableName) ->
        CreateTableCommand = "aws dynamodb create-table --table-name " ++ TableName ++ " 
        --attribute-definitions 
            AttributeName=id,AttributeType=N
        --key-schema 
            AttributeName=id,KeyType=HASH 
        --provisioned-throughput 
            ReadCapacityUnits=5,WriteCapacityUnits=5
        --endpoint " ++ ?ENDPOINT,

    aws_commands:execute_command(CreateTableCommand),
    ok.


get_all_items(TableName, OutputJsonFile) ->
    GetAllRoomsCommand = "aws dynamodb scan --table-name " ++ TableName ++ " --output json > " ++ OutputJsonFile ++ " --endpoint " ++ ?ENDPOINT,
    aws_commands:execute_command(GetAllRoomsCommand),
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