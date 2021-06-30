-module(http_server_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
	Controller = #{id => 'HttpServerController',
				   start => {http_server_controller, start_link, [Port]},
				   restart => permanent,
				   shutdown => 2000,
				   type => worker,
				   modules => [http_server_controller]},

	AcceptorSupervisor = #{id => 'Socket Acceptor Supervisor',
						   start => {http_server_acceptor_sup, start_link, []},
						   restart => permanent,
						   shutdown => 2000,
						   type => supervisor,
						   modules => [http_server_acceptor_sup]},

	{ok, {#{strategy => one_for_all,
			intensity => 5,
			period => 30},
		  [AcceptorSupervisor, Controller]}
	}.
