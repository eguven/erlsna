-module(gatekeeper).
-behaviour(gen_server).

-export([start_gatekeeper/0, add_agent/2, remove_agent/1, clean/0, agent_exists/1,
         find_agent/1, find_multi/1, all_agents/0, all_agents/1, all_relations/0]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%---------------------------------------------------------------------
%%% Description module gatekeeper
%%%---------------------------------------------------------------------
%%% Gatekeeper is a gen_server process responsible of doing record
%%% keeping, matching real-world IDs of agents to PIDs in Erlang runtime
%%%---------------------------------------------------------------------

%% to start gatekeeper outside supervision tree in development
%% gatekeeper:start_link/0 calls this
start_gatekeeper() ->
    gen_server:start_link(?MODULE, [], []).

%%%---------------------------------------------------------------------
%%% Asynchronous calls to gatekeeper
%%%---------------------------------------------------------------------

%% Add an agent to records
%% Args: Id of agent process (real-world identifier)
%%       PID of agent process
add_agent(Id, Pid) ->
    gen_server:cast(whereis(gatekeeper), {add, Id, Pid}).

%% Remove an agent from records
%% Args: Id of agent process
remove_agent(Id) ->
    gen_server:cast(whereis(gatekeeper), {remove, Id}).

%% Remove all records in gatekeeper
clean() ->
    gen_server:cast(whereis(gatekeeper), clean).

%%%---------------------------------------------------------------------
%%% Synchronous calls to gatekeeper
%%%---------------------------------------------------------------------

%% Check if agent with given Id exists
%% Args: ID of agent process
%% Returns: true | false
agent_exists(Id) ->
    gen_server:call(whereis(gatekeeper), {exists, Id}).

%% Find the PID of agent process denoted by given ID
%% Args: ID of agent process
%% Returns: {ok, PID} | {error, not_found}
find_agent(Id) ->
    gen_server:call(whereis(gatekeeper), {find, Id}).

%% Find multiple agents from an ID list or PID list
%% Args: list of IDs or PIDs
%% Returns: {from_ids, [{Idx, Pidx} | ...]}
%%          {from_pids, [{Pidx, Idx} | ...]}
%%          where the second element of the tagged tuple is an orddict
find_multi([HList|TList]) ->
    case is_pid(HList) of
        true -> gen_server:call(whereis(gatekeeper), {find_multi_from_pids, [HList|TList]});
        false -> gen_server:call(whereis(gatekeeper), {find_multi_from_ids, [HList|TList]})
    end.

%% Retrieve a dict of all agents which is the state of gatekeeper
all_agents() ->
    gen_server:call(whereis(gatekeeper), all).

%% Retrieve a list of IDs of all agents
%% Args: id (atom)
%% Returns: [Idx | ...]
all_agents(id) ->
    lists:map(fun({Id,_Pid}) -> Id end, dict:to_list(all_agents()));

%% Retrieve a list of PIDs of all agents
%% Args: pid (atom)
%% Returns: [Pidx | ...]
all_agents(pid) ->
    lists:map(fun({_Id,Pid}) -> Pid end, dict:to_list(all_agents())).

%% This function is shit.
%% Returns a list-of-list-of-tuples where each inner list contains
%% tuples in the form of {SourcePID, DestPID, Value} for all relations that
%% source agent has.
%% There is one list per agent. Empty lists (no relations) are not filtered out
all_relations() ->
    AgentPids = all_agents(pid), % get a list of PIDs
    % {PID, #agent.outdegrees}
    F = fun(P,{{indegrees, _I}, {outdegrees, O}}) -> {P,O} end,
    % [ {PID, #agent.outdegrees} | ...]
    RL = [F(P,agent:get_relations(P)) || P <- AgentPids],
    % S: SourcePID, DList: DestinationList
    BigL = lists:map(fun({S, DList}) ->
        % DPid: DestionationPID, V: RelationValue
        lists:map(fun({DPid, V}) -> {S, DPid, V} end, DList) end, RL),
    
    {relations, BigL}.

%%%---------------------------------------------------------------------
%%% Gatekeeper Process
%%%---------------------------------------------------------------------

init([]) ->
    % register self PID
    register(gatekeeper, self()),
    {ok, dict:new()}.

start_link() ->
    start_gatekeeper().

% extra callbacks
handle_info(Msg, Agent) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Agent}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% terminate function. Just print debug.
terminate(Reason, State) ->
    io:format("Gatekeeper terminated~nReason: ~p~nState:~p~n",[Reason,State]),
    ok.

%%%---------------------------------------------------------------------
%%% Handling Asynchoronous Calls
%%%---------------------------------------------------------------------

%% called from gatekeeper:add_agent/2
handle_cast({add, Id, Pid}, Agents) ->
    {noreply, dict:store(Id, Pid, Agents)};

%% called from gatekeeper:remove_agent/1
handle_cast({remove, Id}, Agents) ->
    {noreply, dict:erase(Id, Agents)};

%% called from gatekeeper:clean/0
handle_cast(clean, _) ->
    {noreply, dict:new()}.

%%%---------------------------------------------------------------------
%%% Handling Synchoronous Calls
%%%---------------------------------------------------------------------

%% called from gatekeeper:agent_exists/1
handle_call({exists, Id}, _From, Agents) ->
    {reply, dict:is_key(Id, Agents), Agents};

%% called from gatekeeper:find_agent/1
handle_call({find, Id}, _From, Agents) ->
    case dict:find(Id, Agents) of
        {ok, Pid} -> {reply, {ok, Pid}, Agents};
        error -> {reply, {error, not_found}, Agents}
    end;

%% called from gatekeeper:find_multi/1. find multiple agents from PidList
handle_call({find_multi_from_pids, PidList}, _From, Agents) ->
    AList = dict:to_list(Agents), % list of {Id, PID}
    % lookup by values, reverse the orddict K/V eg. {Id,Pid} transformed into {Pid,Id}
    F = fun() -> lists:foldl(fun(I,Acc) -> {P,_I} = element(2,lists:keysearch(I,2,AList)),
                                              orddict:store(I,P,Acc) end, orddict:new(), PidList) end,
    try F() of
        RetVal -> {reply, {from_pids, RetVal}, Agents}
    catch
        Type:Exception -> {reply, {error, [Type, Exception]}, Agents}
    end;

%% called from gatekeeper:find_multi/1. find multiple agents from IdList
handle_call({find_multi_from_ids, IdList}, _From, Agents) ->
    % function to foldl over IdList and build orddict of {Id,Pid}
    F = fun() -> lists:foldl(fun(P,Acc) -> orddict:store(P,dict:fetch(P,Agents),Acc) end,
                               orddict:new(), IdList) end,
    try F() of
        RetVal -> {reply, {from_ids, RetVal}, Agents}
    catch
        Type:Exception -> {reply, {error, [Type, Exception]}, Agents}
    end;

%% called from gatekeeper:all_agents/0
handle_call(all, _From, Agents) ->
    {reply, Agents, Agents}.

