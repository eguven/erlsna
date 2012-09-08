-module(eventkeeper).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

init([]) ->
    %register(eventkeeper,self()),
    {ok, []}.

%handle_event({set_teams, TeamA, TeamB}, State) ->
%    curling_scoreboard_hw:set_teams(TeamA, TeamB),
%    {ok, State};

handle_event({new_relation, PidA, PidB, Ts}, State) ->
    io:format("New relation event fired, ~p -> ~p.~n",[PidA, PidB]),
    Tst = erlang:now(),
    % calculate new degree centralities for the two agents and update
    {AI,AO} = algorithms:calc_degree_centrality(PidA),
    agent:update_data(PidA, [AI,AO]),
    {BI,BO} = algorithms:calc_degree_centrality(PidB),
    agent:update_data(PidB, [BI,BO]),
%    AgentPids = gatekeeper:all_agents(pid),
    % Run Floyd-Warshall first
    %floyd_warshall:floyd_warshall(),
    % calculate closeness centrality for agents and cast update

%    lists:map(fun(P)-> {K,V} = algorithms:calc_closeness_centrality(P),
%                       agent:update_data(P,K,V) end, AgentPids),
    % send delayed cast to update history(snapshot) to other agents through events
%    events:delayed_snapshot(lists:delete(PidA,lists:delete(PidB, AgentPids)), 5, "Other relation"),
    % send different note to relation sides
%    events:delayed_snapshot([PidA,PidB], 5, "Self relation"),
%    Ts = erlang:now(),
    agent:update_history(PidA, Ts, "Self Relation"),
    agent:update_history(PidB, Ts, "Self Relation"),
    
    Tend = erlang:now(),
    io:format("> Handle_event new_relation took ~f seconds~n",[util:timestamp_diff(Tst,Tend)]),
    {ok, State};

handle_event({delayed_snapshot, AgentPid, MsDelay, Note}, State) ->
    case is_list(AgentPid) of
        true ->
            % timestamp to send to update cast to sync
            Ts = erlang:now(),
            % function inside map, checks if process alive and casts update
            FF = fun(P) -> case is_process_alive(P) of
                               true -> agent:update_history(P,Ts,Note);
                               _ -> false
                           end end,
            % function with delay to spawn as process
            F = fun() -> timer:sleep(MsDelay),
                         Tst = erlang:now(),
                         %io:format("Entering update casts, L: ~p~n~p~n",[length(AgentPid),AgentPid]),
                         lists:map(FF, AgentPid),
                         Tend = erlang:now(), Td = util:timestamp_diff(Tst,Tend),
                         io:format("Update casts with L: ~p took ~f seconds~n",[length(AgentPid),Td])
                         end,
            
            spawn(F);

        false ->
            F = fun() -> timer:sleep(MsDelay),
                         case is_process_alive(AgentPid) of
                            true -> agent:update_history(AgentPid, Note);
                            _ -> false
                         end end,
            spawn(F)
    end,
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    %io:format("~n**Eventkeeper terminated~nReason: ~p~nState:~p~n",[Reason,State]),
    ok.
