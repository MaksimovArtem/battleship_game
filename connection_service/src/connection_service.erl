-module(connection_service).

-export([connect/1, get_free_clients/1, send_invitation/2]). 

%%Interface module for battleship application

connect(FromNodeName) ->
	connection_server:connect(FromNodeName).


get_free_clients(FromNodeName) ->
	connection_server:get_free_clients(FromNodeName).


send_invitation(TargetNode, InitialNode) ->
	connection_server:send_invitation(TargetNode, InitialNode).