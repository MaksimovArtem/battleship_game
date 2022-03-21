-module(game_server).

-behaviour(gen_server).

-export([start_link/2]).

-export([get_field/1]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {player1 :: atom(),
				player2 :: atom(),
				field1 = [] :: list(),
				field2 = [] :: list()}).

-include("include/battleship.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Player1, Player2) ->
	io:format("~nstarted!~n"),
	gen_server:start_link({global,get_server_name(Player1, Player2)}, ?MODULE, [Player1, Player2], []).


get_field(Player) ->
	case connection_service:get_current_opponent(Player) of
		{ok, {first, Player2}} -> 
			gen_server:call({global,get_server_name(Player, Player2)}, {get_field, Player});
		{ok, {second, Player1}} ->
			gen_server:call({global,get_server_name(Player1, Player)}, {get_field, Player});
		_Other -> {error, "You are not the part of this game!~n"}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Player1, Player2]) ->
	InitField =
	fun() ->
		[[0 || _X <- lists:seq(1,?FIELD_SIZE)] || _Y <- lists:seq(1,?FIELD_SIZE)]
	end,
	{ok, #state{player1 = Player1,
			    player2 = Player2,
			    field1  = InitField(),
			    field2  = InitField()}}.
%%--------------------------------------------------------------


handle_call({get_field, Player}, _From, State = #state{player1 = Player}) ->
	{reply, State#state.field1, State};
handle_call({get_field, Player}, _From, State = #state{player2 = Player}) ->
	{reply, State#state.field2, State}.
%%--------------------------------------------------------------


handle_cast(_Request, State) ->
	{noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_server_name(Player1, Player2) ->
	list_to_atom("game_server_" ++ atom_to_list(Player1) ++ "_" ++ atom_to_list(Player2)).