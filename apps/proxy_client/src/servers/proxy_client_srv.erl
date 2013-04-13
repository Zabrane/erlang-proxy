%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_client_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(RETRY_TIMES, 2).

%% sock: client's local socket server
-record(state, {sock}).

-include("proxy_defs.hrl").
-define(SOCK_OPTIONS,
        [binary,
         {reuseaddr, true},
         {active, false},
         {nodelay, true}
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ConfFile = filename:join(code:priv_dir(proxy_client), "client.conf"),
    case file:consult(ConfFile) of
        {ok, Conf} ->
            ListenPort = proplists:get_value(listen_port, Conf),
            ListenIP = proplists:get_value(listen_ip, Conf),
            {ok, Socket} = gen_tcp:listen(ListenPort, [{ip, ListenIP} | ?SOCK_OPTIONS]),
            {ok, #state{sock=Socket}, 0};
        {error, _} ->
            {stop, conf_file_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{sock=Socket}) ->
    accept_loop(Socket);                        % never return
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{sock=Socket}) ->
    gen_tcp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept_loop(Socket) ->
    {ok, Client} = gen_tcp:accept(Socket),
    %% {ok, Pid} = proxy_client_worker_sup:start_child(Client),
    %% case gen_tcp:controlling_process(Client, Pid) of
    %%     ok ->
    %%         ok;
    %%     {error, Reason} ->
    %%         ?LOG("~p, accept_loop controlling_process error: ~p~n", [Client, Reason])
    %% end,
    %% here we distingush `try` and `retry`, make try = retry + 1
    start_worker_and_hand_over(Client, ?RETRY_TIMES + 1),
    accept_loop(Socket).

start_worker_and_hand_over(_Client, 0) ->
    ?LOG("start_worker_and_hand_over ~p max retry over~n", [_Client]),
    {error, max_retry};
start_worker_and_hand_over(Client, Trys) ->
    {ok, Pid} = supervisor:start_child(proxy_client_worker_sup, [Client]),
    case gen_tcp:controlling_process(Client, Pid) of
        ok ->
            {ok, Pid};
        {error, closed} ->
            ?LOG("start_worker_and_hand_over ~p client socket closed!~n", [Client]),
            supervisor:terminate_child(proxy_client_worker_sup, Pid),
            {error, client_closed};
        {error, _Reason} ->
            supervisor:terminate_child(proxy_client_worker_sup, Pid),
            ?LOG("start_worker_and_hand_over ~p set controlling_process error: ~p, retry!~n", [Client, _Reason]),
            start_worker_and_hand_over(Client, Trys - 1)
    end.
