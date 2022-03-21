-module(game_sup).
-behavior(supervisor).


-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
	SupFlags = #{strategy    => simple_one_for_one,
				 intensity   => 1,
				 period      => 5},
	ChildSpecs = [#{id       => game_server,
				    start    => {game_server, start_link, []},
				    restart  => transient,
				    shutdown => 5000}],
	{ok, {SupFlags, ChildSpecs}}.