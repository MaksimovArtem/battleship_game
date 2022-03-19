-module(battleship_sup).
-behavior(supervisor).


-export([start_link/0]).
-export([init/1]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
	SupFlags = #{strategy   => one_for_one,
				 intensity  => 1,
				 period     => 5},
	ChildSpecs = [#{id      => connection_server,
				   start    => {connection_server, start_server, []},
				   restart  => transient,
				   shutdown => 5000}],
	{ok, {SupFlags, ChildSpecs}}.