-module(http_server_test_stand_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([run_load/2]).


-define(PORT, 8000).

start(_Type, _Args) ->
	ok = application:ensure_started(inets),
	ok = application:ensure_started(http_server),
	observer:start(),
	http_server_test_stand_sup:start_link(?PORT).

stop(_State) ->
	ok.


%%%===================================================================
%%% Test stand API
%%%===================================================================

%%%-----------------------------------------------------------------------------
%%% Start a number of connections to the server. The connections are
%%% created with the specified rate.
%%%
%%% Example:
%%% run_load(100, 10) starts to create 10 connections per second until
%%% 100 of connections is created.
%%%
%%% When all connections are done the report is generated
%%%-----------------------------------------------------------------------------
-spec run_load(integer(), integer()) -> ok.

run_load(TotalNumber, NumberPerSecond) ->
	gen_server:cast(http_server_test_stand, {run, TotalNumber, NumberPerSecond}).