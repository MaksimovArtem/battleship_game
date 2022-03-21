-module(game_service).

-export([start_new_game/2, get_field/1]). 


%%Interface module for game_service application

start_new_game(Player1Node, Player2Node) ->
	whereis(game_sup) == undefined andalso
    begin
		GameSupSpec = #{id       => game_sup,
				   	    start    => {game_sup, start_link, []},
				        restart  => transient,
				        type     => supervisor,
				        shutdown => 5000},
		io:format("~nStarting game_superviser~n",[]),
		supervisor:start_child(battleship_sup, GameSupSpec)
    end,

    %GameSpec = #{id       => game_server,
    %             start    => {game_server, start_link, [Player1Node, Player2Node]},
    %             restart  => transient,
    %             shutdown => 5000},
    io:format("~nStarting worker for existed sup~n",[]),
    supervisor:start_child(game_sup, [Player1Node, Player2Node]).

get_field(PlayerNode) ->
	game_server:get_field(PlayerNode).


%%Internal
