-module(proxy_server_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {server_ip,
                server_port,
                socks_options,
                socket = undefined
               }).

-define(SOCK_OPTIONS,
        [binary,
         {reuseaddr, true},
         {active, once},
         {nodelay, true}
        ]).


start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

init(Config) ->
    ServerIP = proplists:get_value(server_ip, Config),
    ServerPort = proplists:get_value(server_port, Config),
    SocketOptions = proplists:get_value(socket_option,?SOCK_OPTIONS),
    State = #state{
               server_ip = ServerIP,
               server_port = ServerPort,
               socket_options = SocketOptions
           },
    {ok,State,0}.

handle_call(_Request, _From, State) ->
    {reply,{error,unknown_message}, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{server_sock=RemoteSocket, client_sock=Client, client_ip=LocalIP, client_port=LocalPort} = State) ->
%    try
        case find_target(Client) of
            {ok, Mod, {connect, Addr}} ->
                Target = encode_addr(Addr),
                ok = gen_tcp:send(RemoteSocket, proxy_transform:transform(Target)),
                ok = inet:setopts(Client, [{active, true}]),
                IP = list_to_binary(tuple_to_list(getaddr_or_fail(LocalIP))),
                ok = gen_tcp:send(Client, Mod:unparse_connection_response({granted, {ipv4, IP, LocalPort}})),
                {noreply, State};
            {error, client_closed} ->
                {stop, normal, State};
            {error, Reason} ->
                ?LOG("client communication init error: ~p~n", [Reason]),
                {stop, Reason, State}
    %%     end
    %% catch
    %%     error:{badmatch,_} ->
    %%         {stop, normal, State};
    %%     _Error:_Reason ->
    %%         ?LOG("client recv error, ~p: ~p~n", [_Error, _Reason]),
    %%         {stop, normal, State}
    end;
handle_info({tcp, Client, Request}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case gen_tcp:send(RemoteSocket, proxy_transform:transform(Request)) of
        ok ->
            {noreply, State};
        {error, _Error} ->
            {stop, _Error, State}
    end;
handle_info({tcp, RemoteSocket, Response}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case gen_tcp:send(Client, proxy_transform:transform(Response)) of
        ok ->
            {noreply, State};
        {error, _Error} ->
            {stop, _Error, State}
    end;
handle_info({tcp_closed, ASocket}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case ASocket of
        Client ->
            {stop, normal, State};
        RemoteSocket ->
            {stop, normal, State}
    end;
handle_info({tcp_error, ASocket, _Reason}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case ASocket of
        Client ->
            ?LOG("~p client tcp error~n", [ASocket]),
            {stop, _Reason, State};
        RemoteSocket ->
            ?LOG("~p server tcp error~n", [ASocket]),
            {stop, _Reason, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason,  #state{socket = Socket}) ->
    case Socket of
        undefined->
            ok;
        _->
            tcp:close(Socket),
            ok;
    end;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
getaddr_or_fail(IP) ->
    {ok, Addr} = inet:getaddr(IP, inet),
    Addr.


find_target(Client) ->
    %% 0x05:version
    case gen_tcp:recv(Client, 0) of
        {ok, <<Version:8, _/binary>> = Greeting} ->
            socks_proxy_handshake(Client, Version, Greeting);
        {error, closed} ->
            {error, client_closed};
        {error, Reason} ->
            {error, Reason}
    end.

socks_proxy_handshake(Client, Version, Greeting) ->
    case Version of
        %% SOCKS4
        16#04 ->
            case proxy_proto_socks4:parse_greeting_request(Greeting) of
                {connect, _UserId, Addr} ->
                    {ok, proxy_proto_socks4, {connect, Addr}};
                {error, Reason} ->
                    {error, Reason}
            end;
        16#05 ->
            {auth_methods, _} = proxy_proto_socks5:parse_greeting_request(Greeting),
            gen_tcp:send(Client, proxy_proto_socks5:unparse_greeting_response(no_auth)),
            {ok, ConnReq} = gen_tcp:recv(Client, 0),
            case proxy_proto_socks5:parse_connection_request(ConnReq) of
                {connect, Addr} ->
                    {ok, proxy_proto_socks5, {connect, Addr}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

encode_addr({ipv4, Address, Port}) ->
    <<?IPV4, Port:16, Address:32>>;
encode_addr({ipv6, Address, Port}) ->
    <<?IPV6, Port:16, Address:128>>;
encode_addr({domain, DomainBin, Port}) ->
    <<?DOMAIN, Port:16, (byte_size(DomainBin)):8, DomainBin/binary>>;
encode_addr(_) ->
    error.
