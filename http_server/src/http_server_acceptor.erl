-module(http_server_acceptor).

-export([start_link/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(ControllerPid, ListeningSocket, Opts) ->
	Pid = spawn_link(fun() -> do_accept(ControllerPid, ListeningSocket, Opts) end),
	{ok, Pid}.

%%%-----------------------------------------------------------------------------
%%% First stage of acceptor process. Wait until connection via `ListeningSocket'
%%% established, then configure accepted `Socket' to decode incoming data as
%%% `http_bin' and wait for data.
%%%-----------------------------------------------------------------------------
-spec do_accept(pid(), port(), map()) -> no_return().

do_accept(ControllerPid, ListeningSocket, Opts) ->
	case catch gen_tcp:accept(ListeningSocket) of
		{ok, Socket} ->
			ControllerPid ! {{acceptor, self()}, accepted},
			ok = inet:setopts(Socket, [{packet, http_bin}]),
			do_receive(ControllerPid, Socket, Opts, [{headers, []}]);
		{error, Reason} ->
			exit(Reason)
	end.

%%%-----------------------------------------------------------------------------
%%% Second stage of acceptor process. After connection has been accepted then
%%% configure acceptor process to receive only one message from `Socket'.
%%% When correct HTTP message comes, parse this one message by message recursively.
%%% When whole message is received and decoded then handle it.
%%%-----------------------------------------------------------------------------
-spec do_receive(pid(), port(), map(), list()) -> no_return().

do_receive(ControllerPid, Socket, Opts, RequestElements) ->
	ok = inet:setopts(Socket, [{active, once}]),
	HttpMsg = receive
				  {http, Socket, Msg} ->
					  Msg;
				  {tcp_closed, _} ->
					  gen_tcp:close(Socket),
					  exit(tcp_closed)
			  after 5000 ->
			gen_tcp:close(Socket),
			exit(normal)
			  end,
	case HttpMsg of
		{http_request, M, {abs_path, Uri}, Vsn} ->
			NRequestElements = [{method, M}, {uri, Uri}, {version, Vsn} | RequestElements],
			do_receive(ControllerPid, Socket, Opts, NRequestElements);
		{http_header, _, Hdr, _, Val} ->
			{headers, Hdrs} = lists:keyfind(headers, 1, RequestElements),
			do_receive(ControllerPid, Socket, Opts, lists:keystore(headers, 1, RequestElements,
																   {headers, [{Hdr, Val} | Hdrs]}));
		http_eoh ->
			do_handle_request(ControllerPid, Socket, Opts, RequestElements);
		{http_error, Error} ->
			gen_tcp:close(Socket),
			exit(Error)
	end.

%%%-----------------------------------------------------------------------------
%%% Third stage of acceptor process. When HTTP request is received then
%%% handle the one. Then job is started and the acceptor process waits
%%% until one of the following happens:
%%%
%%% 1) Job is done during of the allowed time.
%%% 2) Some error happens during of Job execution
%%% 3) Job is terminated due to some reason
%%% 4) Job spent more than allowed time and timeout was triggered
%%% 5) Client has closed the connection
%%%
%%% All cases except the last one produce corresponding HTTP
%%% response and send it back to the client.
%%%
%%% There is no keep-alive support then after response is sent
%%% the connection is closed and the acceptor process is terminated.
%%%-----------------------------------------------------------------------------
-spec do_handle_request(pid(), port(), map(), list()) -> no_return().

do_handle_request(_ControllerPid, Socket, Opts, _RequestElements) ->
	ok = inet:setopts(Socket, [{active, once}]),
	SelfPid = self(),
	JobTimeout = maps:get(job_timeout, Opts),
	JobFile = maps:get(job_file, Opts),
	JobFun = maps:get(job_function, Opts, fun do_job/3),
	JobPid = spawn_link(fun() ->
		try
			JobFun(SelfPid, JobFile, JobTimeout)
		catch
			_:Reason ->
				SelfPid ! {do_job, {error, io_lib:format("do_job exception: ~p", [Reason])}}
		end
						end),
	Ref = erlang:monitor(process, JobPid),

	{Status, PhraseArgs} = receive
							   {do_job, done} ->
								   {200, "Job was done"};
							   {do_job, {error, Reason}} ->
								   erlang:demonitor(Ref, [flush]),
								   {500, Reason};
							   {'DOWN', Ref, process, JobPid, _Reason} ->
								   {500, "Internal error"};
							   {tcp_closed, _} ->
								   gen_tcp:close(Socket),
								   exit(tcp_closed)
						   after JobTimeout ->
			exit(JobPid, timeout),
			erlang:demonitor(Ref, [flush]),
			{503, "Overloaded"}
						   end,
	ok = inet:setopts(Socket, [{packet, raw}]),
	ok = gen_tcp:send(Socket, http_server_util:assembly_http_response(Status, PhraseArgs)),
	gen_tcp:close(Socket),
	exit(normal).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-----------------------------------------------------------------------------
%%% Job function. Just try to read the file and notify the acceptor
%%% process about what is happened.
%%%-----------------------------------------------------------------------------
-spec do_job(pid(), list(), integer()) -> ok.

do_job(HandlerPid, JobFile, _JobTimeout) ->
	case file:read_file(JobFile) of
		{ok, _Binary} ->
			HandlerPid ! {?FUNCTION_NAME, done};
		{error, Reason} when is_atom(Reason) ->
			HandlerPid ! {?FUNCTION_NAME, {error, atom_to_list(Reason)}}
	end.