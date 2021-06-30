-module(http_server_test_stand).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		 code_change/3]).

-define(SERVER, ?MODULE).
-define(COUNTER_DB, counter_db).

-record(analyzer_state, {
	port = undefined :: port(),
	connections = [] :: list({pid(), reference()}),
	started = false :: boolean()}
).

-record(run_load_counters, {
	sent = 0 :: non_neg_integer(),
	succ = 0 :: non_neg_integer(),
	fail = 0 :: non_neg_integer(),
	'200' = 0 :: non_neg_integer(),
	'500' = 0 :: non_neg_integer(),
	'503' = 0 :: non_neg_integer()
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================
start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []).

init(Port) ->
	ets:new(?COUNTER_DB, [named_table, public, set, {keypos, 1},
						  {write_concurrency, true}]),
	{ok, #analyzer_state{port = Port}}.

handle_call(_Request, _From, State = #analyzer_state{}) ->
	{reply, ok, State}.

handle_cast({run, _TotalNumber, _NumberPerSecond}, State = #analyzer_state{started = true}) ->
	io:format("The test is ongoing. Please wait!~n", []),
	{noreply, State};

handle_cast({run, TotalNumber, NumberPerSecond}, State = #analyzer_state{port = Port}) ->
	ets:insert(?COUNTER_DB, {run_load_start, os:timestamp()}),
	ets:insert(?COUNTER_DB, #run_load_counters{}),
	DelayBetweenConnectionsList = erlang:float_to_list((1000 / NumberPerSecond), [{decimals, 0}]),
	DelayBetweenConnectionsMs = erlang:list_to_integer(DelayBetweenConnectionsList),
	ConnPidMonList = [begin
						  timer:sleep(DelayBetweenConnectionsMs),
						  spawn_monitor(fun() -> do_request(Port) end)
					  end || _ <- lists:seq(1, TotalNumber)],

	{noreply, State#analyzer_state{connections = ConnPidMonList, started = true}};

handle_cast(_Request, State = #analyzer_state{}) ->
	{noreply, State}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State = #analyzer_state{connections = ConnPidMonList}) ->
	NewState = case lists:delete({Pid, Ref}, ConnPidMonList) of
				   [] ->
					   % run_load is done
					   ets:insert(?COUNTER_DB, {run_load_end, os:timestamp()}),
					   generate_report(),
					   State#analyzer_state{connections = [], started = false};
				   L ->
					   State#analyzer_state{connections = L}
			   end,
	{noreply, NewState};

handle_info(_Info, State = #analyzer_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #analyzer_state{}) ->
	ets:delete(?COUNTER_DB),
	ok.

code_change(_OldVsn, State = #analyzer_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
inc_cnt(Type) ->
	Pos = case Type of
			  sent -> #run_load_counters.sent;
			  succ -> #run_load_counters.succ;
			  fail -> #run_load_counters.fail;
			  200 -> #run_load_counters.'200';
			  500 -> #run_load_counters.'500';
			  503 -> #run_load_counters.'503'
		  end,
	ets:update_counter(?COUNTER_DB, run_load_counters, {Pos, 1}).

get_http_msg_status_code({{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, _Body}) ->
	StatusCode.

do_request(Port) ->
	inc_cnt(sent),
	case httpc:request(get, {"http://localhost:" ++ integer_to_list(Port), []}, [],
					   [{sync, true}]) of
		{ok, Result} ->
			inc_cnt(succ),
			inc_cnt(get_http_msg_status_code(Result));
		{error, _Reason} ->
			inc_cnt(fail)
	end.

format_utc_timestamp(TS) ->
	{{Year, Month, Day}, {Hour, Minute, Second}} =
	calendar:now_to_universal_time(TS),
	Mstr = element(Month, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
						   "Aug", "Sep", "Oct", "Nov", "Dec"}),
	io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w",
				  [Day, Mstr, Year, Hour, Minute, Second]).

generate_report() ->
	[{run_load_start, StartTime}] = ets:lookup(?COUNTER_DB, run_load_start),
	[{run_load_end, EndTime}] = ets:lookup(?COUNTER_DB, run_load_end),
	[Counters] = ets:lookup(?COUNTER_DB, run_load_counters),
	io:format("~n", []),
	io:format("--------- REPORT START ---------~n", []),
	io:format("Started at: ~s~n", [format_utc_timestamp(StartTime)]),
	io:format("Ended at: ~s~n", [format_utc_timestamp(EndTime)]),
	io:format("--------------------------------~n", []),
	io:format("Counters:~n", []),
	io:format("   Total requests sent: ~p~n", [Counters#run_load_counters.sent]),
	io:format("   Successfull requests: ~p~n", [Counters#run_load_counters.succ]),
	io:format("   Failed requests: ~p~n", [Counters#run_load_counters.fail]),
	io:format("   Requests with status code 200: ~p~n", [Counters#run_load_counters.'200']),
	io:format("   Requests with status code 500: ~p~n", [Counters#run_load_counters.'500']),
	io:format("   Requests with status code 503: ~p~n", [Counters#run_load_counters.'503']),
	io:format("---------- REPORT END ----------~n", []),
	ok.
