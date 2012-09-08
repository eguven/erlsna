-module(gatekeeper).
-behaviour(gen_server).

-export([start_gatekeeper/0, add_agent/2, remove_agent/1, clean/0, agent_exists/1,
         find_agent/1, find_multi/1, all_agents/0, all_agents/1, all_relations/0]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_gatekeeper() ->
    gen_server:start_link(?MODULE, [], []).

start_link() ->
    start_gatekeeper().
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_agent(Id, Pid) ->
    gen_server:cast(whereis(gatekeeper), {add, Id, Pid}).

remove_agent(Id) ->
    gen_server:cast(whereis(gatekeeper), {remove, Id}).

clean() ->
    gen_server:cast(whereis(gatekeeper), clean).

agent_exists(Id) ->
    gen_server:call(whereis(gatekeeper), {exists, Id}).

find_agent(Id) ->
    gen_server:call(whereis(gatekeeper), {find, Id}).

find_multi([HList|TList]) ->
    case is_pid(HList) of
        true -> gen_server:call(whereis(gatekeeper), {find_multi_from_pids, [HList|TList]});
        false -> gen_server:call(whereis(gatekeeper), {find_multi_from_ids, [HList|TList]})
    end.

all_agents() ->
    gen_server:call(whereis(gatekeeper), all).

all_agents(id) ->
    lists:map(fun({Id,_Pid}) -> Id end, dict:to_list(all_agents()));
all_agents(pid) ->
    lists:map(fun({_Id,Pid}) -> Pid end, dict:to_list(all_agents())).

% returns list-of [{Source, Dest, Val},...]
all_relations() ->
    AgentPids = all_agents(pid),
    F = fun(P,{{indegrees, _I}, {outdegrees, O}}) -> {P,O} end,
    RL = [F(P,agent:get_relations(P)) || P <- AgentPids],
    % S: Source DList: DestinationList
    BigL = lists:map(fun({S, DList}) ->
        lists:map(fun({DPid, V}) -> {S, DPid, V} end, DList) end, RL),
    
    {relations, BigL}.

init([]) ->
    % this ets is currently used for update tracking, not related to gatekeeper
    %ets:new(updates_ets, [set, public, named_table]),
    %ets:insert(updates_ets, {running, false}),
    % end
    register(gatekeeper, self()),
    {ok, dict:new()}.

handle_cast({add, Id, Pid}, Agents) ->
    {noreply, dict:store(Id, Pid, Agents)};

handle_cast({remove, Id}, Agents) ->
    {noreply, dict:erase(Id, Agents)};

handle_cast(clean, _) ->
    {noreply, dict:new()}.

handle_call({exists, Id}, _From, Agents) ->
    {reply, dict:is_key(Id, Agents), Agents};

handle_call({find, Id}, _From, Agents) ->
    case dict:find(Id, Agents) of
        {ok, Pid} -> {reply, {ok, Pid}, Agents};
        error -> {reply, {error, not_found}, Agents}
    end;

% find multiple agents from PidList
handle_call({find_multi_from_pids, PidList}, _From, Agents) ->
    AList = dict:to_list(Agents),
    % lookup by values, reverse the orddict K/V eg. {Id,Pid} transformed into {Pid,Id}
    F = fun() -> lists:foldl(fun(I,Acc) -> {P,_I} = element(2,lists:keysearch(I,2,AList)),
                                              orddict:store(I,P,Acc) end, orddict:new(), PidList) end,
    try F() of
        RetVal -> {reply, {from_pids, RetVal}, Agents}
    catch
        Type:Exception -> {reply, {error, [Type, Exception]}, Agents}
    end;

% find multiple agents from IdList
handle_call({find_multi_from_ids, IdList}, _From, Agents) ->
    % function to foldl over IdList and build orddict of {Id,Pid}
    F = fun() -> lists:foldl(fun(P,Acc) -> orddict:store(P,dict:fetch(P,Agents),Acc) end,
                               orddict:new(), IdList) end,
    try F() of
        RetVal -> {reply, {from_ids, RetVal}, Agents}
    catch
        Type:Exception -> {reply, {error, [Type, Exception]}, Agents}
    end;

handle_call(all, _From, Agents) ->
    {reply, Agents, Agents}.

% extra callbacks
handle_info(Msg, Agent) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Agent}.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

terminate(Reason, State) ->
    io:format("Gatekeeper terminated~nReason: ~p~nState:~p~n",[Reason,State]),
    ok.
