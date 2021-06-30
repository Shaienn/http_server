-module(http_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 8000).
-define(JOB_FILE, "/tmp/download_test").
-define(TIMEOUT, 5000).

start(_StartType, StartArgs) ->
	ParamsList = [{port, ?PORT}, {job_file, ?JOB_FILE}, {job_timeout, ?TIMEOUT}, {job_function, undefined}],

	Opts = lists:foldl(fun({Parameter, DefaultValue}, CurrOpts) ->
		case lists:keyfind(Parameter, 1, StartArgs) of
			{Parameter, Value} ->
				CurrOpts#{Parameter => Value};
			false ->
				case {application:get_env(http_server, Parameter), DefaultValue} of
					{undefined, undefined} ->
						CurrOpts;
					{{ok, Value}, _} ->
						CurrOpts#{Parameter => Value};
					{undefined, Value} ->
						CurrOpts#{Parameter => Value}
				end
		end
					   end, #{}, ParamsList),

	io:format("OPTS: ~p~n", [Opts]),
	http_server_sup:start_link(Opts).

stop(_State) ->
	ok.

%% internal functions
