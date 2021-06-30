-module(http_server_acceptor_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	AcceptorChild = #{id => 'AcceptorWorker',
			   start => {http_server_acceptor, start_link, []},
			   restart => temporary,
			   shutdown => 2000,
			   type => worker,
			   modules => [http_server_acceptor]},

	{ok, {#{strategy => simple_one_for_one},
		  [AcceptorChild]}
	}.
