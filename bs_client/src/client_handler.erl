-module(client_handler).

%%client side of the game

-export([connect/0, get_free_clients/0, send_invitation/1, waiting_for_invitation/0]).
-export([set_ship/0, attack/0]).

-export([client_name/0]).

-define(SERVER_NAME, 'server@192.168.0.103').
-define(CLIENT_NAME, node()).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   API for connection_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect() ->
	net_adm:ping(?SERVER_NAME),
	ListenerPid = spawn(?MODULE, waiting_for_invitation, []), %%To be supervised?
	global:register_name(client_name(), ListenerPid),
	rpc:call(?SERVER_NAME, connection_service, connect, [?CLIENT_NAME]).


get_free_clients() ->
	rpc:call(?SERVER_NAME, connection_service, get_free_clients, [?CLIENT_NAME]).


send_invitation(TargetNode) ->
	rpc:call(?SERVER_NAME, connection_service, send_invitation, [TargetNode, ?CLIENT_NAME]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   API for game_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_ship() ->
	ok.


attack() ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

client_name() ->
	list_to_atom("client_" ++ atom_to_list(?CLIENT_NAME)).
%%--------------------------------------------------------------


waiting_for_invitation() ->
	receive
		{invitation, Text} ->
			io:format(Text);
		Other ->
			io:format("Other ~p~n",[Other])
	end.