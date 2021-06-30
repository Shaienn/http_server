-module(http_server_create_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([all/0]).
-export([job_function_pass/1, job_function_timeout/1, job_function_error/1, job_function_exit/1, job_function_throw/1]).

all() ->
	[job_function_pass, job_function_timeout, job_function_error, job_function_exit, job_function_throw].

init_per_testcase({_, Type}, Config) ->
	ok = application:ensure_started(inets),
	Fun = case Type of
			  pass -> fun pass_test_fun/3;
			  timeout -> fun timeout_test_fun/3;
			  error -> fun error_test_fun/3;
			  exit -> fun exit_test_fun/3;
			  throw -> fun throw_test_fun/3
		  end,
	{ok, Pid} = http_server_app:start(suite, [{job_timeout, 1000}, {job_function, Fun}]),
	[{http_server, Pid} | Config].

end_per_testcase(_, Config) ->
	exit(?config(http_server, Config), normal).

job_function_pass(Config) ->
	Test = {?FUNCTION_NAME, pass},
	TestData = init_per_testcase(Test, Config),
	{ok, Res} = httpc:request("http://localhost:8000"),
	ct:pal(?LOW_IMPORTANCE, "---> ~p~n", [Res]),
	200 = get_http_msg_status_code(Res),
	end_per_testcase(Test, TestData).

job_function_timeout(Config) ->
	Test = {?FUNCTION_NAME, timeout},
	TestData = init_per_testcase(Test, Config),
	{ok, Res} = httpc:request("http://localhost:8000"),
	ct:pal(?LOW_IMPORTANCE, "---> ~p~n", [Res]),
	503 = get_http_msg_status_code(Res),
	end_per_testcase(Test, TestData).

job_function_error(Config) ->
	Test = {?FUNCTION_NAME, error},
	TestData = init_per_testcase(Test, Config),
	{ok, Res} = httpc:request("http://localhost:8000"),
	ct:pal(?LOW_IMPORTANCE, "---> ~p~n", [Res]),
	500 = get_http_msg_status_code(Res),
	end_per_testcase(Test, TestData).

job_function_exit(Config) ->
	Test = {?FUNCTION_NAME, exit},
	TestData = init_per_testcase(Test, Config),
	{ok, Res} = httpc:request("http://localhost:8000"),
	ct:pal(?LOW_IMPORTANCE, "---> ~p~n", [Res]),
	500 = get_http_msg_status_code(Res),
	end_per_testcase(Test, TestData).

job_function_throw(Config) ->
	Test = {?FUNCTION_NAME, throw},
	TestData = init_per_testcase(Test, Config),
	{ok, Res} = httpc:request("http://localhost:8000"),
	ct:pal(?LOW_IMPORTANCE, "---> ~p~n", [Res]),
	500 = get_http_msg_status_code(Res),
	end_per_testcase(Test, TestData).

%%%===================================================================
%%% Internal functions
%%%===================================================================
pass_test_fun(HandlerPid, _JobFile, _JobTimeout) ->
	ct:pal(?LOW_IMPORTANCE, "~p is called~n", [?FUNCTION_NAME]),
	HandlerPid ! {do_job, done}.

timeout_test_fun(HandlerPid, _JobFile, _JobTimeout) ->
	ct:pal(?LOW_IMPORTANCE, "~p is called~n", [?FUNCTION_NAME]),
	timer:sleep(5000),
	HandlerPid ! {do_job, done}.

error_test_fun(_HandlerPid, _JobFile, _JobTimeout) ->
	ct:pal(?LOW_IMPORTANCE, "~p is called~n", [?FUNCTION_NAME]),
	erlang:error(error_exception).

exit_test_fun(_HandlerPid, _JobFile, _JobTimeout) ->
	ct:pal(?LOW_IMPORTANCE, "~p is called~n", [?FUNCTION_NAME]),
	exit(exit_exception).

throw_test_fun(_HandlerPid, _JobFile, _JobTimeout) ->
	ct:pal(?LOW_IMPORTANCE, "~p is called~n", [?FUNCTION_NAME]),
	throw(throw_exception).

get_http_msg_status_code({{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}) ->
	StatusCode.