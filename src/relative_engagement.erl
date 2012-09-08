-module(relative_engagement).
-export([relative_engagement/2]).

-include("records.hrl").

relative_engagement(AgentPid, TimeStep) ->
    % agent's engagement throughout its lifetime
    AgentEng = analysis_engagement:engagement(AgentPid, {0,0,0},
                                              erlang:now(), TimeStep),
    Aggregate = aggregate_engagement:aggregate_engagement(TimeStep),
    % agent's first
    T_zero = element(1,lists:nth(1,AgentEng)),
    % 1 TimeStep ago
    T_pre = util:sub_seconds_from_ts(T_zero,TimeStep),
    % Function to filter 1 TimeStep prior
    PrevFilter = fun({Ts,_I,_O}) -> (T_pre < Ts) and (Ts =< T_zero) end,
    {PrevStep,Rest} = lists:partition(PrevFilter, Aggregate),
    % agent's last
    T_last = element(1,lists:nth(length(AgentEng),AgentEng)),
    % 1 TimeStep after
%    T_post = util:add_seconds_to_ts({0,0,0},T_last+TimeStep),
    % Function to filter 1 TimeStep after
%    PostFilter = fun({Ts,_I,_O}) -> (T_last < Ts) and (Ts =< T_post) end,
%    {PostStep,Rest2} = lists:partition(PostFilter, Rest),
    % Lastly filter Rest2 to get the agent's lifetime from aggregate
    ActualFilter = fun({Ts,_I,_O}) -> (T_zero < Ts) and (Ts =< T_last) end,
    Aggregate2 = lists:filter(ActualFilter, Rest),
    Str = "Agent first: ~p Agent length: ~p Aggregate length: ~p~n",
    io:format(Str, [T_zero,length(AgentEng),length(Aggregate2)]),
    io:format("Compounding...~n"),
    {NetworkEng, AgentEng2} = compound_measures(Aggregate2, AgentEng),
    Str2 = "Agent first: ~p Agent length: ~p~nCompounded : ~p / ~p~n",
    io:format(Str2,[element(1,hd(AgentEng2)),length(AgentEng2),
                    element(1,hd(NetworkEng)),length(NetworkEng)]),

    % set past months data as t0 for Network
    SumTmp = sum_engagements(PrevStep),
    NewHead = {element(1,hd(NetworkEng)),element(1,SumTmp),element(2,SumTmp)},
    NewNetworkEng = [NewHead | tl(NetworkEng)],
    {{network, NewNetworkEng},{agent,AgentEng2}}.

% merge individuals' values
compound_measures(NetworkList, AgentList) ->
    helper([], [], lists:keysort(1,NetworkList),
           lists:keysort(1, AgentList), []).

% Compunded, Temporary, NetworkList, AgentToDo, AgentDone
% Compounded and AgentDone grows using cons ( | ), needs reverse when done

% done
helper(Compounded, [], [], [], AgentDone) ->
    {lists:reverse(Compounded), lists:reverse(AgentDone)};

% NetworkList done
helper(Compounded, Temporary, [], AgentToDo, AgentDone) ->
    io:format("NetworkList done, length(AgentToDo) should be =< 1 (is ~p)~n",
              [length(AgentToDo)]),
    % time to sum temporary
    SumTmp = sum_engagements(Temporary),
    % use agent's timestamp
    New = {element(1,hd(AgentToDo)),element(1,SumTmp),element(2,SumTmp)},
    NewCompounded = [New | Compounded],
    helper(NewCompounded, [], [], tl(AgentToDo),
           [hd(AgentToDo) | AgentDone]);

% keep adding from NetworkList to Temporary while Timestamp is =< agent's
helper(Compounded, Temporary, NetworkList, AgentToDo, AgentDone) when
    element(1,hd(NetworkList)) =< element(1,hd(AgentToDo)) ->

    % add to temporary to be summed later
    helper(Compounded, [hd(NetworkList) | Temporary], tl(NetworkList),
           AgentToDo, AgentDone);

% agent's timestamp reached in NetworkList, sum temporary and add to Compounded
helper(Compounded, Temporary, NetworkList, AgentToDo, AgentDone) when
    element(1,hd(NetworkList)) > element(1,hd(AgentToDo)) ->

    % time to sum temporary
    SumTmp = sum_engagements(Temporary),
    % use agent's timestamp
    New = {element(1,hd(AgentToDo)),element(1,SumTmp),element(2,SumTmp)},
    NewCompounded = [New | Compounded],
    helper(NewCompounded, [], NetworkList, tl(AgentToDo),
           [hd(AgentToDo) | AgentDone]).
    


% sum timestamped engagement values, eg. [{Ts,{in_diff,I},{out_diff,O}}, {...}]
sum_engagements(EngList) ->
    % sum function
    F = fun({_Ts,{in_diff,I},{out_diff,O}}, {{in_diff,AccI},{out_diff,AccO}}) ->
            {{in_diff,AccI+I},{out_diff,AccO+O}} end,
    lists:foldl(F,{{in_diff,0},{out_diff,0}},EngList).

















