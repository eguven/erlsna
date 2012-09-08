-module(agent_util).
-export([make_snapshot/5, update_history/3, format_snapshot/2,
         print_snapshot/2, print_history/1, sort_history/1,
         degree_centralities/1, update_degree_centralities/1,
         snapshot_sum/2]).
-include("records.hrl").

% timestamp: now() , note: update cause
%-record(snapshot, {timestamp, note, indegrees, outdegrees, data}).

make_snapshot(Timestamp, Note, Indegrees, Outdegrees, Data) ->
    #snapshot{timestamp=Timestamp,note=Note,indegrees=Indegrees,
                outdegrees=Outdegrees,data=Data}.

update_history(Agent = #agent{}, Timestamp, Note) ->
    %{{_,I},{_,O}} = agent:get_relations(Agent#agent.pid),
    I = Agent#agent.indegrees,
    O = Agent#agent.outdegrees,
    % add degree centralities to data -> Atemp
    %Atemp = update_degree_centralities(Agent),
    %Atemp = update_closeness_centrality(Atemp1),

    NewSnapshot = make_snapshot(Timestamp,Note,I,O,Agent#agent.data),
    NewHistory = [NewSnapshot | Agent#agent.history],
    NewAgent = Agent#agent{history=NewHistory},
    NewAgent.

format_snapshot(S = #snapshot{}, AgentId) ->
    io_lib:format("Agent ~p on ~p~nReason: ~p~nIndegrees: ~p~nOutdegrees: ~p~nData: ~p~n~n",
        [AgentId,S#snapshot.timestamp,S#snapshot.note,S#snapshot.indegrees,
         S#snapshot.outdegrees,S#snapshot.data]).

print_snapshot(S = #snapshot{}, AgentId) ->
    io:format(format_snapshot(S, AgentId)),
    ok.

print_history(Agent = #agent{}) ->
    L = lists:foldr(fun(S,IoList) -> IoList++format_snapshot(S,Agent#agent.id) end,
                        [], Agent#agent.history),
    io:format(L),
    ok.

sort_history(Agent = #agent{}) ->
    % ordering function, order on timestamp, descending
    F = fun(A, B) -> A#snapshot.timestamp > B#snapshot.timestamp end,
    NewHistory = lists:sort(F, Agent#agent.history),
    NewAgent = Agent#agent{history=NewHistory},
    NewAgent.

% return indegree/outdegree centralities for the agent
degree_centralities(Agent = #agent{}) ->
    I = length(Agent#agent.indegrees),
    O = length(Agent#agent.outdegrees),
    {{indegree, I},{outdegree, O}}.

% update agent data with current indegree/outdegree centrality values
update_degree_centralities(Agent = #agent{}) ->
    {{_,I},{_,O}} = degree_centralities(Agent),
    NewData = orddict:store(indegree_centrality,I,
                orddict:store(outdegree_centrality,O,Agent#agent.data)),
    NewAgent = Agent#agent{data=NewData},
    NewAgent.


% sum indegree/outdegree weights on snapshot
snapshot_sum(indegrees, Snapshot) ->
    L = orddict:to_list(Snapshot#snapshot.indegrees),
    lists:foldl(fun({_P,V}, Sum) -> V+Sum end, 0, L);
    
snapshot_sum(outdegrees, Snapshot) ->
    L = orddict:to_list(Snapshot#snapshot.outdegrees),
    lists:foldl(fun({_P,V}, Sum) -> V+Sum end, 0, L).












