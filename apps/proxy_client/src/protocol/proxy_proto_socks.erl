-module(proxy_proto_socks).
-hehaviour(ranch_protocol).

-export([start_link/4]).

-export([init/4, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{
          ref,
          socket,
          transport,
          step,
          buf
         }).

start_link(ListenerPid, Socket, Transport, Opts)->
    proc_lib:start_link(?MODULE,init,[ListenerPid,Socket,Transport,Opts]).

init(Ref,Socket,Transport,_Opts = []) ->
    ok = proc_lib:init_ack({ok,self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket,[{active,once}]),
    State = #state{ref = Ref,socket = Socket,transport = Transport,step = init,buf = <<>>},
    gen_server:enter_loop(?MODULE,[],State).

handle_call(_Request, _From, State) ->
    {reply, {error,unknown_message}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp,Socket,Data},State = #state{socket = Socket,transport = Transport,
                                         step = Step,buf = Buf})->
    Transport:setopts(Socket,{active,once}),
    NewBuf = <<Buf/bits,Data/bits>>,
    {NewStep,Command,Rest} = protocol_socks5:parse_protocol(Step,NewBuf),
    case Command of
        undefined ->    
            ok;
        _->
            gen_server:cast(self(),Command)
    end,
    {noreply,State#state{step = NewStep,buf = Rest}};
handle_info({tcp_clsoed,Socket},State = #state{socket = Socket})->
    {stop,normal};
handle_info({tcp_error,Socket},State = #state{socket = Socket,transport = Transport})->
    Transport:close(Socket),
    {stop,normal};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason,#state{socket = Socket,transport = Transport}) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

