-module(proxy_task_manager).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(task,{
          uuid,
          pid,
          data
         }).

-record(state {
          socks_owners,
          tasks
         }).
start_link() ->
    gen_server:start_link({local,?MODULE}, [], []).
init([]) ->
    SocksOwners = ets:new(socks_owners, [set, private]),
    Tasks = ets:new(tasks,[set,private,
                          {keypos,#task.uuid}]),
    State = #state{
               socks_owners = SocksOwners,
               tasks = Tasks
              }
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error,unknown_message}, State}.

handle_cast({remote_data,UUID,Data},State = #state{tasks = Tasks})->
    MatchHead = #task{uuid = UUID,pid = '$1',_='_'},
    Matched = ets:select(Name,[{MatchHead,[],['$1']}]),
    lists:foreach(fun(Pid) ->
                          gen_server:cast(Pid,{remote_data,Data})
                  end,Matched),
    {noreply, State};  

handle_cast({local_data,Pid,Data},
            State = #state{socks_owner = SocksOwners,tasks = Tasks})-> 
    UUID = hm_uuid:uuid_utc(),
    Task = #task{
              uuid = UUID,
              pid = Pid,
              data = Data
             },
    true = ets:insert(Tasks,Task),                                
    hm_misc:monitor(Pid,SocksOwners),
    pool_utils:cast(server_pool,{local_data,UUID,Data}),
    {noreply,State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, _Type, Pid, _Info},
            State = #state{socks_owner = SocksOwners,tasks = Tasks})-> 
    hm_misc:demonitor(Pid,SocksOwners),
    MatchHead = #task{uuid = '$1',pid = Pid,_='_'},
    Matched = ets:select(Name,[{MatchHead,[],['$1']}]),
    lists:foreach(fun(Key) ->
                          ets:delete(Tasks, Key),
                  end,Matched),
    {noreply, State};  

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
