-module(http_server_test_stand_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) ->
	HttpServerTestStand = #{id => 'HttpServerTestStand',
							start => {http_server_test_stand, start_link, [Port]},
							restart => permanent,
							shutdown => 2000,
							type => worker,
							modules => [http_server_test_stand]},

	{ok, {#{strategy => one_for_one,
			intensity => 5,
			period => 30},
		  [HttpServerTestStand]}
	}.