%%%---------------------------------------------------------------------
%%% Description module agent
%%%---------------------------------------------------------------------
%%% Agents are gen_server processes representing an agent/node in the
%%% network.
%%%---------------------------------------------------------------------

-module(agent).
-behaviour(gen_server).
-include("records.hrl").

-export([create_agent/1, delete_agent/1, make_agent/1, make_agent/2, make_agent/3,
         add_relation/2, add_relation/3, add_relation/4, adjust_relation/3, adjust_relation/4,
         get_relations/1, remove_relation/2,
         update_data/2, update_data/3, update_history/2, update_history/3, agent_info/2]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% TODO: remove TS from add/remove _from functions, necessary events are synced

%% Create an agent record with an Id
make_agent(Id) -> make_agent(Id, orddict:new()).
%% Create an agent record with an Id and additional data (orddict)
make_agent(Id, Data) -> make_agent(Id, erlang:now(), Data).
%% Create an agent record with an Id, creation timestamp and data
make_agent(Id, Created, Data) -> #agent{id=Id, created=Created, data=Data}.

%% Add agent to gatekeeper (see gatekeeper.erl) for recordkeeping
register_agent(Agent) ->
    gatekeeper:add_agent(Agent#agent.id, Agent#agent.pid).

%% Remove agent from gatekeeper
unregister_agent(Agent) ->
    gatekeeper:remove_agent(Agent#agent.id).

%% Tell supervisor to start agent
create_agent(Agent) ->
    agent_sup:start_agent(Agent).

%% Terminate agent process and delete agent
delete_agent(AgentPid) ->
    gen_server:call(AgentPid, terminate).

% - UPDATE: this is not allowed < R14B03
%delete_agent(AgentPid) ->
%    agent_sup:stop_agent(AgentPid).

%%%---------------------------------------------------------------------
%%% Synchronous calls to agent
%%%---------------------------------------------------------------------

%% get agent's relations (indegrees and outdegrees)
%% Args: PID of agent process
%% Returns: { {indegrees, orddict()}, {outdegrees, orddict()} }
get_relations(AgentPid) ->
    {I, O} = gen_server:call(AgentPid, relations),
    {{indegrees, I}, {outdegrees, O}}.

%% get arbitrary info from agent data with orddict key
%% Args: PID of agent process
%%       key of data (atom)
%% Returns: Value
agent_info(AgentPid, Key) ->
    gen_server:call(AgentPid, {get_info, Key}).

%%%---------------------------------------------------------------------
%%% Asynchronous calls to agent
%%%---------------------------------------------------------------------

%% add a relation (outdegree) to agent with relation value (edge weight) and TS
%% Args: PID of source agent process
%%       PID of target agent process
%%       Numerical value/weight of relation
%%       timestamp() of creation
add_relation(AgentPid, TargetPid, RelationValue, Timestamp) ->
    gen_server:cast(AgentPid, {add_outdegree, TargetPid, RelationValue, Timestamp}).

%% same as add_relation/4 without timestamp (default is erlang:now/0)
add_relation(AgentPid, TargetPid, RelationValue) ->
    add_relation(AgentPid, TargetPid, RelationValue, erlang:now()).

%% same as add_relation/3 without relation value (default is 1)
add_relation(AgentPid, TargetPid) ->
    add_relation(AgentPid, TargetPid, 1).

%% adjust the value of a relation
%% Args: PID of source agent process
%%       PID of target agent process
%%       Numerical change (+/-)
%%       timestamp() for change
adjust_relation(AgentPid, TargetPid, Adjustment, Timestamp) ->
    gen_server:cast(AgentPid, {adjust_relation, TargetPid, Adjustment, Timestamp}).

%% same as adjust_relation/4 without timestamp (default is erlang:now/0)
adjust_relation(AgentPid, TargetPid, Adjustment) ->
    adjust_relation(AgentPid, TargetPid, Adjustment, erlang:now()).

%% add_relation_from
add_relation_from(AgentPid, SourcePid, RelationValue, Timestamp) ->
    gen_server:cast(AgentPid, {add_indegree, SourcePid, RelationValue, Timestamp}).

remove_relation(AgentPid, TargetPid) ->
    gen_server:cast(AgentPid, {remove_outdegree, TargetPid}).

%% remove_relation_from
remove_relation_from(AgentPid, SourcePid, Timestamp) ->
    gen_server:cast(AgentPid, {remove_indegree, SourcePid, Timestamp}).

%% update data
update_data(AgentPid, Key, Value) ->
    gen_server:cast(AgentPid, {update_data, Key, Value}).

%% update data k,v pairs
update_data(AgentPid, [H|T]) ->
    lists:map(fun({K,V}) -> update_data(AgentPid, K, V) end, [H|T]).

%% update_history
update_history(AgentPid, Note) ->
    gen_server:cast(AgentPid, {update_history, Note}).

%% update_history with timestamp
update_history(AgentPid, Timestamp, Note) ->
    gen_server:cast(AgentPid, {update_history, Timestamp, Note}).

% Agent Process
%init([Agent|_]) ->
%    NewAgent = Agent#agent{pid=self()},
%    register_agent(NewAgent),
%    {ok, NewAgent}.
init(Agent) ->
    %Ts = erlang:now(),
    NewAgentTemp = Agent#agent{pid=self()},
    NewAgent = agent_util:update_history(NewAgentTemp,Agent#agent.created,"Created"),
    register_agent(NewAgent),
    {ok, NewAgent}.

start_link(Agent) ->
    gen_server:start_link(?MODULE, Agent, []).

% handle_call
handle_call(terminate, _From, Agent) ->
    {stop, normal, ok, Agent};

handle_call(relations, _From, Agent) ->
    Reply = {Agent#agent.indegrees, Agent#agent.outdegrees},
    {reply, Reply, Agent};

handle_call({get_info, id}, _From, Agent) ->
    {reply, Agent#agent.id, Agent};

handle_call({get_info, all}, _From, Agent) ->
    {reply, Agent, Agent};

% TODO: change to tagged return
handle_call({get_info, Key}, _From, Agent) ->
    case orddict:find(Key, Agent#agent.data) of
        {ok, Value} -> {reply, Value, Agent};
        error -> {reply, {error, not_found}, Agent}
    end.

%%% handle_cast

%% add_outdegree, called from agent:add_relation
handle_cast({add_outdegree, TargetPid, RelationValue, Timestamp}, Agent) ->
    Outdegrees = Agent#agent.outdegrees,
    NewOutdegrees = orddict:store(TargetPid, RelationValue, Outdegrees),
    % create Timestamp to match add_indegree and add_outdegree
    %Ts = erlang:now(),
    % Ts in add_relation_from is not used, synced update in events:new_relation
    add_relation_from(TargetPid, Agent#agent.pid, RelationValue, Timestamp),
    NewAgent = Agent#agent{outdegrees=NewOutdegrees},
    %%%% UPDATE HISTORY
    %events:delayed_snapshot([Agent#agent.pid,TargetPid], 5, "New relation"),
    events:new_relation(Agent#agent.pid,TargetPid, Timestamp),
    %NewAgent = agent_util:update_history(NewAgentTemp, Ts, "Added outdegree"),
    %update_history(Agent#agent.pid, Ts, "Added outdegree"),
    {noreply, NewAgent};

%% add_indegree, called from agent:handle_cast({add_outdegree, _, _}, _)
%% add_relation_from
handle_cast({add_indegree, SourcePid, RelationValue, _Timestamp}, Agent) ->
    Indegrees = Agent#agent.indegrees,
    NewIndegrees = orddict:store(SourcePid, RelationValue, Indegrees),
    NewAgent = Agent#agent{indegrees=NewIndegrees},
    %%%% UPDATE HISTORY
    %%% -> update moved to synced event in add_outdegree
    %NewAgent = agent_util:update_history(NewAgentTemp, Timestamp, "Added indegree"),
    %update_history(Agent#agent.pid, Timestamp, "Added indegree"),
    {noreply, NewAgent};


%% remove_outdegree, called from agent:remove_relation
handle_cast({remove_outdegree, TargetPid}, Agent) ->
    NewOutdegrees = orddict:erase(TargetPid, Agent#agent.outdegrees),
    % create Timestamp to match remove_indegree and remove_outdegree
    Ts = erlang:now(),
    remove_relation_from(TargetPid, Agent#agent.pid, Ts),
    NewAgent = Agent#agent{outdegrees=NewOutdegrees},
    %%%% UPDATE HISTORY
    events:new_relation(Agent#agent.pid,TargetPid),
    %events:delayed_snapshot([Agent#agent.pid,TargetPid], 5, "Removed relation"),
    %NewAgent = agent_util:update_history(NewAgentTemp, Ts, "Removed outdegree"),
    %update_history(Agent#agent.pid, Ts, "Removed outdegree"),
    {noreply, NewAgent};

%% remove_indegree, called from agent:handle_cast({remove_outdegree, _}, _)
%% remove_relation_from
handle_cast({remove_indegree, SourcePid, _Timestamp}, Agent) ->
    NewIndegrees = orddict:erase(SourcePid, Agent#agent.indegrees),
    NewAgent = Agent#agent{indegrees=NewIndegrees},
    %%%% UPDATE HISTORY
    %%% -> update moved to synced event in remove_outdegree
    %NewAgent = agent_util:update_history(NewAgentTemp, Timestamp, "Removed indegree"),
    %update_history(Agent#agent.pid, Timestamp, "Removed indegree"),
    {noreply, NewAgent};

%% update some key in data
handle_cast({update_data, Key, Value}, Agent) ->
    NewData = orddict:store(Key,Value,Agent#agent.data),
    NewAgent = Agent#agent{data=NewData},
    {noreply, NewAgent};

%% update history
handle_cast({update_history, Note}, Agent) ->
    Ts = erlang:now(),
    NewAgent = agent_util:update_history(Agent, Ts, Note),
    {noreply, NewAgent};

%% update history with timestamp
handle_cast({update_history, Timestamp, Note}, Agent) ->
    NewAgent = agent_util:update_history(Agent, Timestamp, Note),
    {noreply, NewAgent};

%% adjust_relation
handle_cast({adjust_relation, TargetPid, Adjustment, Timestamp}, Agent) ->
    %try orddict:fetch(TargetPid, Agent#agent.outdegrees) of
    case orddict:find(TargetPid, Agent#agent.outdegrees) of
        {ok, Val} ->
            NewValue =  Val + Adjustment,
            NewOutdegrees = orddict:store(TargetPid, NewValue, Agent#agent.outdegrees),
            NewAgent = Agent#agent{outdegrees=NewOutdegrees},
            %Ts = erlang:now(),
            % Ts in add_relation_from is not used, synced update in events:new_relation
            add_relation_from(TargetPid, Agent#agent.pid, NewValue, Timestamp),
            %%%% UPDATE HISTORY
            events:new_relation(Agent#agent.pid, TargetPid, Timestamp),
            {noreply, NewAgent};
    %catch
    %    _Type:_Exception ->
            %io:format("adjust_relation on ~p, invalid target", [Agent#agent.id]),
            % create relation
    %        {noreply, Agent}
        error ->
            add_relation(Agent#agent.pid, TargetPid, Adjustment, Timestamp),
            {noreply, Agent}
    end;


%% forget_agent
handle_cast({forget_agent, TargetPid}, Agent) ->
    NewOutdegrees = orddict:erase(TargetPid, Agent#agent.outdegrees),
    NewIndegrees = orddict:erase(TargetPid, Agent#agent.indegrees),
    NewAgent = Agent#agent{indegrees=NewIndegrees, outdegrees=NewOutdegrees},
    %Ts = erlang:now(),
    %%%% UPDATE HISTORY
    events:delayed_snapshot(Agent#agent.pid, 5, "Forgotten agent"),
    %NewAgent = agent_util:update_history(NewAgentTemp, Ts, "Forgotten agent"),
    {noreply, NewAgent}.

%% pre_terminate, called from terminate to cleanup relations
pre_terminate(Agent) ->
    IndegreePids = lists:map(fun({K,_V}) -> K end, Agent#agent.indegrees),
    OutdegreePids = lists:map(fun({K,_V}) -> K end, Agent#agent.outdegrees),
    Recipients = lists:usort(lists:append(IndegreePids, OutdegreePids)),
    io:format("Sending forget_agent to Recipients ~p~n", [Recipients]),
    [gen_server:cast(P, {forget_agent, Agent#agent.pid}) ||
        P <- Recipients],
    ok.

% terminate
terminate(normal, Agent = #agent{}) ->
    % robustness check
    io:format("Self: ~p~n", [self()]),
    io:format("Terminating ~p with PID ~p~n",[Agent#agent.id,Agent#agent.pid]),
    %if self() /= Agent#agent.pid -> erlang:error(agent_pid_mismatch) end,
    case self() =:= Agent#agent.pid of
        false -> erlang:error(agent_pid_mismatch);
        true -> io:format("Terminated: ~p with PID ~p~n",[Agent#agent.id,Agent#agent.pid])
    end,
    % tell indegrees and outdegrees to remove self
    pre_terminate(Agent),
    unregister_agent(Agent).

% extra callbacks
handle_info(Msg, Agent) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Agent}.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

