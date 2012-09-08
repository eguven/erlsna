-module(aggregate_engagement).
-export([aggregate_engagement/1,add_agent_to_total/3]).

-include("records.hrl").

% accumulated values at every timestamp separated be the amount of TimeStep
aggregate_engagement(TimeStep) ->
    EndTs = erlang:now(),
    AllPids = gatekeeper:all_agents(pid),
    AgentPid = lists:nth(random:uniform(length(AllPids)),AllPids),
    AgentEng = analysis_engagement:engagement(AgentPid, {0,0,0},
                                              EndTs, TimeStep),
    AgentsRest = lists:delete(AgentPid, AllPids),
    % aggregate function to fold over all agents' engagement
    F = fun(APid,Acc) ->
        NewAgentEng = analysis_engagement:engagement(APid,{0,0,0},
                                                     EndTs,TimeStep),
        add_agent_to_total([],NewAgentEng,Acc)
        end,

    lists:foldl(F,AgentEng,AgentsRest).


% add_agent_to_total: Merged Agent Acc
% add agent's engagement to the accumulated value
add_agent_to_total(Merged,
                   [{TsAgent,{in_diff,IAgent},{out_diff,OAgent}} | TAgent],
                   [{TsAcc,{in_diff,IAcc},{out_diff,OAcc}} | TAcc])
                   when TsAcc < TsAgent ->
    % skipping times before agents lifetime
    add_agent_to_total([{TsAcc,{in_diff,IAcc},{out_diff,OAcc}} | Merged],
                       [{TsAgent,{in_diff,IAgent},{out_diff,OAgent}} | TAgent],
                       TAcc);

add_agent_to_total(Merged,
                   [{TsAgent,{in_diff,IAgent},{out_diff,OAgent}} | TAgent],
                   [{TsAcc,{in_diff,IAcc},{out_diff,OAcc}} | TAcc])
                   when TsAcc == TsAgent ->
%    io:format("EQUAL: ~p~n", [TsAcc]),
    % add values at equal timepoint
    add_agent_to_total([{TsAcc,{in_diff, IAcc+IAgent},{out_diff,OAcc+OAgent}} |
                            Merged],
                       TAgent,
                       TAcc);
        
add_agent_to_total(Merged,
                   [{TsAgent,{in_diff,IAgent},{out_diff,OAgent}} | TAgent],
                   [{TsAcc,{in_diff,IAcc},{out_diff,OAcc}} | TAcc])
                   when TsAcc > TsAgent ->
    % this agent is older than the current start of Acc
    add_agent_to_total([{TsAgent,{in_diff,IAgent},{out_diff,OAgent}} | Merged],
                       TAgent,
                       [{TsAcc,{in_diff,IAcc},{out_diff,OAcc}} | TAcc]);

% Acc done
add_agent_to_total(Merged,
                   [{TsAgent,{in_diff,IAgent},{out_diff,OAgent}} | TAgent],
                   []) ->
    % This agent has activity succeeding Acc's latest activity
    add_agent_to_total(
        lists:reverse([{TsAgent,{in_diff,IAgent},{out_diff,OAgent}} | TAgent] ++
            Merged),
        [],[]);

% agent done
add_agent_to_total(Merged, [],
                   [{TsAcc,{in_diff,IAcc},{out_diff,OAcc}} | TAcc]) ->
    add_agent_to_total(
        lists:reverse([{TsAcc,{in_diff,IAcc},{out_diff,OAcc}} | TAcc]) ++
            Merged,
        [],[]);

% all done
add_agent_to_total(Merged, [],[]) ->
    lists:reverse(Merged).



















