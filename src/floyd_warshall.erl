-module(floyd_warshall).
-export([floyd_warshall/0]).
%%% Implementation of Floyd-Warshall algorithm

%% list of nodes, sorted (here, agent pids)
all_nodes() ->
    %N = lists:sort(gatekeeper:all_agents(pid)),
    %fun() -> N end.
    lists:sort(gatekeeper:all_agents(pid)).

%% init dists for node, all infinity except self = 0
init_node_dists(AgentPid) ->
    D1 = lists:foldl(fun(P,Acc) -> orddict:store(P,infinity,Acc) end,
                     orddict:new(), all_nodes()),
    orddict:store(AgentPid,0,D1).

%% initialize distance list of a node including inexistent edges with weight
%% infinity, and return as tuple of tuples
init_node(AgentPid) ->
    % agent outdegrees
    {{_,_},{_,O}} = agent:get_relations(AgentPid),
    % fill in agents outdegrees
    D = lists:foldl(fun({P,D},Acc) -> orddict:store(P,D,Acc) end,
                    init_node_dists(AgentPid),
                    orddict:to_list(O)),
    ets:insert(dists, {AgentPid, D}).

%% 2D matrix lookup adjusted for ETS
path(I,J) ->
    AgentDists = element(2,hd(ets:lookup(dists,I))),
    orddict:fetch(J,AgentDists).

%% 2D matrix set adjusted for ETS
set_path(I,J,Val) ->
    AgentDists = element(2,hd(ets:lookup(dists,I))),
    D = orddict:store(J,Val,AgentDists),
    ets:insert(dists, {I, D}).

%% create new ETS table, initialize with node distances
initialize_ets(NodeList) ->
    case lists:member(dists, ets:all()) of
        true ->
            ets:delete(dists),
            ets:new(dists, [set, public, named_table]);
        false ->
            ets:new(dists, [set, public, named_table])
    end,
    lists:map(fun(P) -> init_node(P) end, NodeList).
    
floyd_warshall() ->
    io:format(">>> FW run~n",[]),
    N = all_nodes(),
    initialize_ets(N),
    fw_helper(N).

fw_helper(N) ->
    lists:map(fun(K) ->
        lists:map(fun(I) ->
            lists:map(fun(J) ->
                %set_path(I,J, erlang:min( path(I,J), path(I,K)+path(K,J) ))
                set_helper(K,I,J)
            end, N)
        end, N)
    end, N).

%% infinity in arithmetic operation fails, so
set_helper(K,I,J) ->
    Current = path(I,J),
    New1 = path(I,K),
    New2 = path(K,J),
    try New1+New2 of
        Val -> set_path(I,J, erlang:min( Current, Val ))
    catch
        % infinity in addition
        _:_ -> {no_change, {K,I,J}}
    end.












