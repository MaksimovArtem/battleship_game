-module(connection_service).

-export([connect/1, get_free_clients/1, send_invitation/2]). 


%%Interface module for connection_service application

connect(FromNodeName) ->
	connection_server:connect(FromNodeName).


get_free_clients(FromNodeName) ->
	connection_server:get_free_clients(FromNodeName).


send_invitation(TargetNode, InitialNode) ->
	connection_server:send_invitation(TargetNode, InitialNode).


get_current_opponent(FromNodeName) ->
	connection_server:get_current_opponent(FromNodeName).