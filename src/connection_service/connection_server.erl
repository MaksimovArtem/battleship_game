-module(connection_server).
-behaviour(gen_server).

-export([connect/1, get_free_clients/1, send_invitation/2, get_current_opponent/1]).
-export([start_link/0]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2]).

-record(state, {clients = [] :: list(),
				active_requests = [] ::list(),
				pairs_in_game = [] :: list()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link({global,?MODULE}, ?MODULE, [], []).


connect(FromNodeName) ->
	gen_server:call({global, ?MODULE}, {connect, FromNodeName}).


get_free_clients(FromNodeName) ->
	gen_server:call({global, ?MODULE}, {get_free_clients, FromNodeName}).


get_current_opponent(FromNodeName) ->
	gen_server:call({global, ?MODULE}, {get_current_opponent, FromNodeName}).


send_invitation(TargetNode, InitialNode) ->
	gen_server:cast({global, ?MODULE}, {send_invitation, TargetNode, InitialNode}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	{ok, #state{}}.
%%--------------------------------------------------------------


handle_call({connect, FromNodeName}, _From, State = #state{clients=Clients}) ->
	{reply, ok, State#state{clients = [FromNodeName|Clients]}};

handle_call({get_free_clients, RequesterNodeName}, _From, State = #state{clients=Clients,pairs_in_game=Busy}) ->
	BusyClients = extract_busy_clients(Busy),
	{reply, {ok, (Clients -- BusyClients) -- [RequesterNodeName]}, State};

handle_call({get_current_opponent, FromNodeName}, _From, State = #state{pairs_in_game = InGame}) ->
	SearchFun =
	fun Search(_P, []) -> nothing;
		Search(Player, [{Player, P2}|_]) -> {first,  P2};
		Search(Player, [{P1, Player}|_]) -> {second, P1};
		Search(Player, [{_,_}|T]) -> Search(Player, T)
	end,
	Reply =
	case SearchFun(FromNodeName, InGame) of
		Output = {_MyPos, Player} when is_atom(Player) -> {ok, Output};
		Other -> {error, Other}
	end, 
	{reply, Reply, State}.
%%--------------------------------------------------------------


handle_cast({send_invitation, TargetNode, Requester}, State = #state{active_requests = Requests}) ->
	IsInitial = is_initial_request(Requests, TargetNode, Requester),
	InvitationText = wanna_play_text(IsInitial, Requester),
	global:send(get_registered_name(TargetNode), {invitation, InvitationText}),
	update_state_after_invitation(IsInitial, TargetNode, Requester, State).
%%--------------------------------------------------------------


handle_continue({start_new_game, TargetNode, Requester}, State) ->
	game_service:start_new_game(TargetNode, Requester),
	{noreply, State}.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_busy_clients(Busy) ->
	extract_busy_clients(Busy,[]).

extract_busy_clients([],Acc) ->
	Acc;
extract_busy_clients([{Client1, Client2}|T],Acc) ->
	extract_busy_clients(T,[Client1, Client2|Acc]).
%%--------------------------------------------------------------


is_initial_request([], _Target, _Requester) ->
	true;
is_initial_request([{Target, Requester}|_], Target, Requester) ->
	false;
is_initial_request([_Request|T], Target, Requester) ->
	is_initial_request(T, Target, Requester).
%%--------------------------------------------------------------


wanna_play_text(_IsInitial = true, Requester) ->
"
Node " ++ atom_to_list(Requester) ++ " wants to play with you!
Send invitation back to start the game.
";
wanna_play_text(false, Requester) ->
"
Node " ++ atom_to_list(Requester) ++ " ready to play with you!
Starting the game.
".
%%--------------------------------------------------------------


update_state_after_invitation(true, TargetNode, Requester, State = #state{active_requests = Requests}) ->
	{noreply, State#state{active_requests = [{Requester, TargetNode}|Requests]}};

update_state_after_invitation(false, TargetNode, Requester, State = #state{active_requests = Requests,
																		   pairs_in_game = InGame}) ->
	{noreply, State#state{active_requests = Requests -- [{TargetNode, Requester}],
						  pairs_in_game = [{Requester, TargetNode}|InGame]},
	 					 {continue, {start_new_game,TargetNode, Requester}}}.
%%--------------------------------------------------------------


get_registered_name(NodeName) ->
	list_to_atom("client_" ++ atom_to_list(NodeName)).
%%--------------------------------------------------------------