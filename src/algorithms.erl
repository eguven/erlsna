-module(algorithms).
-export([dijkstra/1, degree_centrality/1, closeness_centrality/1, engagement_analysis/1]).
-export([relative_engagement/1]).
-export([calc_degree_centrality/1, calc_closeness_centrality/1]).
-include("records.hrl").

init_distances() ->
    [{Pid, infinity} || Pid <- gatekeeper:all_agents(pid)].

init_prevs() ->
    [{Pid, undefined} || Pid <- gatekeeper:all_agents(pid)].

%init_queue() ->
%    [{Weight, Pid} || {Pid, Weight} <- init_distances()].

dijkstra([Source]) ->
    {ok, SourcePid} = gatekeeper:find_agent(Source),
    dijkstra(SourcePid);
% Source :: Pid
dijkstra(Source) ->
    Dists = orddict:store(Source, 0, orddict:from_list(init_distances())),
    Prevs = orddict:from_list(init_prevs()),
    QTemp = [{Weight, Pid} || {Pid, Weight} <- orddict:to_list(Dists)],
    Queue = ordsets:from_list(QTemp),
    %io:format("Dists: ~p~n",[Dists]),
    %io:format("Prevs: ~p~n",[Prevs]),
    %io:format("Queue: ~p~n",[Queue]),
    d_helper(Source, Dists, Prevs, Queue).

% Queue empty, all vertices done
d_helper(_Source, Dists, _Prevs, []) ->
    Dists;

% Connected vertices are done, next vertex with least dist has Dist infinity
d_helper(_Source, Dists, _Prevs, [{infinity,_U}|_QTail]) ->
    Dists;

% U: Vertex with least distance in Queue
% get neigbours of U and relax
d_helper(Source, Dists, Prevs, [{Ud,U}|QTail]) ->
    % neighbours of U still in Queue (will ne RelaxList below)
    {{indegrees, _I}, {outdegrees, O}} = agent:get_relations(U),
    N = lists:filter(fun({Pid,_Weight}) ->
            ordsets:is_element({orddict:fetch(Pid, Dists), Pid}, QTail) end,
            O),
    {NewDists, NewPrevs, NewQueue} = relax_helper(U, Ud, Dists, Prevs, QTail, N),
    d_helper(Source, NewDists, NewPrevs, NewQueue).

% All neighbours of U relaxed
relax_helper(_U, _Ud, Dists, Prevs, Queue, []) ->
    {Dists, Prevs, Queue};

relax_helper(U, Ud, Dists, Prevs, Queue, [{Vpid, Vdist}|RelaxList]) ->
%    Dnew = orddict:fetch(U, Dists) + Vdist,
    Dnew = Ud + Vdist,
    case Dnew < orddict:fetch(Vpid, Dists) of
        true -> 
            NewDists = orddict:store(Vpid, Dnew, Dists),
            NewPrevs = orddict:store(Vpid, U, Prevs),
            Qsub = {orddict:fetch(Vpid, Dists), Vpid},
            NewQueue = ordsets:add_element({Dnew, Vpid}, ordsets:del_element(Qsub, Queue)),
            relax_helper(U, Ud, NewDists, NewPrevs, NewQueue, RelaxList);
        false ->
            relax_helper(U, Ud, Dists, Prevs, Queue, RelaxList)
    end.

%%% Degree Centrality
degree_centrality([Agent]) ->
    {ok, AgentPid} = gatekeeper:find_agent(Agent),
    %d_centrality(SourcePid,{0,0,0},erlang:now());
    RetVal = calc_degree_centrality(AgentPid),
    RetVal;

degree_centrality([Agent, TstartString, TendString]) ->
    Ts = util:timestring_to_timestamp(TstartString),
    Te = util:timestring_to_timestamp(TendString),
    {ok, AgentPid} = gatekeeper:find_agent(Agent),
    get_degree_centralities(AgentPid, Ts, Te).

% Source :: Pid
get_degree_centralities(Agent, Tstart, Tend) ->
    A = agent:agent_info(Agent, all),
    FilterF = fun(S) -> (S#snapshot.timestamp >= Tstart) and (S#snapshot.timestamp =< Tend) end,
    % filter snapshot list for timeframe
    Slist = lists:filter(FilterF, A#agent.history),
    Centralities = [snapshot_d_centrality(S) || S <- lists:reverse(Slist)],
    Centralities.

snapshot_d_centrality(S = #snapshot{}) ->
    %{MeSec,Sec,_MicSec} = S#snapshot.timestamp,
    Tstr = util:timestamp_to_timestring(S#snapshot.timestamp),
    {Tstr, {indegree_centrality, length(S#snapshot.indegrees)},
           {outdegree_centrality, length(S#snapshot.outdegrees)}}.

% current degree centrality values
calc_degree_centrality(AgentPid) ->
    {{indegrees,I},{outdegrees,O}} = agent:get_relations(AgentPid),
    {{indegree_centrality,length(I)},{outdegree_centrality,length(O)}}.

% calculate current closeness_centrality
closeness_centrality([Agent]) ->
    {ok, AgentPid} = gatekeeper:find_agent(Agent),
    RetVal = calc_closeness_centrality(AgentPid),
    %agent:update_data(closeness_centrality, element(2,RetVal)),
    RetVal;

closeness_centrality([Agent, TstartString, TendString]) ->
    Ts = util:timestring_to_timestamp(TstartString),
    Te = util:timestring_to_timestamp(TendString),
    {ok, AgentPid} = gatekeeper:find_agent(Agent),
    get_closeness_centralities(AgentPid, Ts, Te).

% get closeness centrality values within [Tstart, Tend)
% they are retrieved from snapshots, not calculated
get_closeness_centralities(AgentPid, Tstart, Tend) ->
    A = agent:agent_info(AgentPid, all),
    FilterF = fun(S) -> (S#snapshot.timestamp >= Tstart) and (S#snapshot.timestamp =< Tend) end,
    % filter snapshot list for timeframe
    Slist = lists:filter(FilterF, A#agent.history),
    Centralities = [snapshot_c_centrality(S) || S <- lists:reverse(Slist)],
    Centralities.

snapshot_c_centrality(S = #snapshot{}) ->
    {MeSec,Sec,_MicSec} = S#snapshot.timestamp,
    {MeSec*1000000+Sec, {closeness_centrality, orddict:fetch(closeness_centrality,S#snapshot.data)}}.

% current closeness centrality value
calc_closeness_centrality(AgentPid) ->
    % closeness centrality is the inverse of farness which is defined as the sum
    % of shortest paths to all other nodes
    F = fun({_P,Dist},Farness) -> 
            if
                Dist =/= infinity -> Dist+Farness;
                true -> Farness
            end
        end,
    % fold over {Pid, Dist} list
    Far = lists:foldl(F,0,algorithms:dijkstra(AgentPid)),

    %% Testing Floyd-Warshall, assuming the data in ETS is fresh
    %Far = lists:foldl(F,0,ets:lookup_element(dists,AgentPid,2)),

    if
        Far =/= 0 -> {closeness_centrality, 1/Far};
        true -> {closeness_centrality, 0}
    end.

%%% engagement analysis, from analysis_engagement module
engagement_analysis([AgentId, TstartString, TendString, TimestepString]) ->
    {ok, AgentPid} = gatekeeper:find_agent(AgentId),
    Ts = util:timestring_to_timestamp(TstartString),
    Te = util:timestring_to_timestamp(TendString),
    {Tstep,_Rest} = string:to_integer(TimestepString),
    Result = analysis_engagement:engagement(AgentPid, Ts, Te, Tstep),
    Result.

%%% relative engagement, from relative_engagement module
relative_engagement([AgentId, TimestepString]) ->
    {ok, AgentPid} = gatekeeper:find_agent(AgentId),
    {Tstep,_Rest} = string:to_integer(TimestepString),
    Result = relative_engagement:relative_engagement(AgentPid, Tstep),
    Result.



