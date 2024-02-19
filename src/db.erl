-module(db).
-export([create_table/1, get_all_items/2, create_item/2, delete_item/2]).

create_table(TableName) ->
        CreateTableCommand = "aws dynamodb create-table --table-name " ++ TableName ++ " 
        --attribute-definitions 
            AttributeName=id,AttributeType=N
        --key-schema 
            AttributeName=id,KeyType=HASH 
        --provisioned-throughput 
            ReadCapacityUnits=5,WriteCapacityUnits=5
        --endpoint http://localhost:8000",

    aws_commands:execute_command(CreateTableCommand),
    ok.


get_all_items(TableName, OutputJsonFile) ->
    GetAllRoomsCommand = "aws dynamodb scan --table-name " ++ TableName ++ " --output json > " ++ OutputJsonFile ++ " --endpoint http://localhost:8000",
    aws_commands:execute_command(GetAllRoomsCommand),
    ok.


create_item(TableName, InputJsonFile) ->
    CreateRoomCommand = "aws dynamodb put-item " 
        "--table-name " ++ TableName ++ " " 
        "--item file://" ++ InputJsonFile ++ " "
        "--endpoint http://localhost:8000",
    aws_commands:execute_command(CreateRoomCommand),
    ok.


delete_item(TableName, InputJsonFile) ->
    DeleteRoomCommand = "aws dynamodb delete-item " 
        "--table-name " ++ TableName ++ " " 
        "--key file://" ++ InputJsonFile ++ " "
        "--endpoint http://localhost:8000",
    aws_commands:execute_command(DeleteRoomCommand),
    ok.