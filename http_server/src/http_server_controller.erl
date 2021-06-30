-module(http_server_controller).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
		 code_change/3]).

-define(SERVER, ?MODULE).
-define(ACCEPTORS_NUM, 4).

-record(http_server_controller_state, {
	options = #{} :: map(),
	acceptor_list = [] :: [pid()],
	listening_socket = undefined :: port() | undefined}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Opts) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%%%-----------------------------------------------------------------------------
%%% Start of the controller process.
%%%
%%% The controller begins to listen the port and creates
%%% a number of acceptor processes. There are few acceptor processes spawned
%%% simultaneously to handle incoming requests faster. Each acceptor process
%%% is monitored by controller. The list of {AcceptorPid, AcceptorRef} tuples
%%% is stored in the controller state.
%%%-----------------------------------------------------------------------------
-spec init(map()) -> {ok, #http_server_controller_state{}}.

init(Opts) ->
	Port = maps:get(port, Opts),
	TcpOpts = [
		{reuseaddr, true},
		{backlog, 32768},
		{packet, raw},
		{active, false}
	],
	{ok, ListeningSocket} = gen_tcp:listen(Port, TcpOpts),
	AcceptorList = [start_new_acceptor(ListeningSocket, Opts) || _N <- lists:seq(1, ?ACCEPTORS_NUM)],
	{ok, #http_server_controller_state{
		options          = Opts,
		listening_socket = ListeningSocket,
		acceptor_list    = AcceptorList}}.

handle_call(_Request, _From, State = #http_server_controller_state{}) ->
	{reply, ok, State}.

handle_cast(_Request, State = #http_server_controller_state{}) ->
	{noreply, State}.

%%%-----------------------------------------------------------------------------
%%% When an incoming connection is accepted then acceptor process notifies
%%% the controller that it is. To maintain a number of acceptors which are
%%% ready to accept new incoming request an additional acceptor process
%%% is spawned.
%%%-----------------------------------------------------------------------------
handle_info({{acceptor, _AcceptorPid}, accepted}, State = #http_server_controller_state{}) ->
	ListeningSocket = State#http_server_controller_state.listening_socket,
	Opts = State#http_server_controller_state.options,
	AcceptorList = State#http_server_controller_state.acceptor_list,
	NewAcceptorPid = start_new_acceptor(ListeningSocket, Opts),
	{noreply, State#http_server_controller_state{acceptor_list = AcceptorList ++ [NewAcceptorPid]}};

%%%-----------------------------------------------------------------------------
%%% When an acceptor process is terminated due to some reason then monitor
%%% notifies the controller process about it. Terminated acceptor is removed
%%% from the controller state. If number of acceptor processes is less than
%%% minimal then additional one is spawned.
%%%-----------------------------------------------------------------------------
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
	AcceptorList = State#http_server_controller_state.acceptor_list,
	CleanAcceptorList = AcceptorList -- [{Pid, Ref}],
	CorrectAcceptorList = case length(CleanAcceptorList) of
							  L when L < ?ACCEPTORS_NUM ->
								  ListeningSocket = State#http_server_controller_state.listening_socket,
								  Opts = State#http_server_controller_state.options,
								  NewAcceptorPidRef = start_new_acceptor(ListeningSocket, Opts),
								  CleanAcceptorList ++ [NewAcceptorPidRef];
							  _L ->
								  CleanAcceptorList
						  end,
	{noreply, State#http_server_controller_state{acceptor_list = CorrectAcceptorList}};

handle_info(_Info, State = #http_server_controller_state{}) ->
	{noreply, State}.

terminate(_Reason, _State = #http_server_controller_state{}) ->
	ok.

code_change(_OldVsn, State = #http_server_controller_state{}, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-----------------------------------------------------------------------------
%%% Start new acceptor process and it's monitoring
%%%-----------------------------------------------------------------------------
-spec start_new_acceptor(port(), map()) -> {pid(), reference()}.
start_new_acceptor(ListeningSocket, Opts) ->
	case supervisor:start_child(http_server_acceptor_sup, [self(), ListeningSocket, Opts]) of
		{ok, AcceptorPid} ->
			AcceptorRef = erlang:monitor(process, AcceptorPid),
			{AcceptorPid, AcceptorRef};
		{error, Reason} ->
			exit({acceptor_start_error, Reason})
	end.