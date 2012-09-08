-module(analysis_engagement).
-export([engagement/4]).
-include("records.hrl").

% This analysis is to plot changes in indegree/outdegree sums over time
% for agents, use case: analysis of send/receive patterns in NNTP

% Agent :: pid() | AgentId
engagement(AgentPid, Tstart, Tend, TimeStep) ->
    % filter func
    F = fun(S = #snapshot{}) -> (Tstart =< S#snapshot.timestamp) and
                                (S#snapshot.timestamp =< Tend) end,
    Agent = agent:agent_info(AgentPid, all),
    % history is stored in reverse chronological order
    L = lists:reverse(Agent#agent.history),
    % history within timeframe
    TimeframeL = lists:filter(F, L),
    engagement_helper(L, TimeframeL, TimeStep).

% set previous values to start with the analysis at first time point
% as if it is run contiguous, eg. positive diffs at initial if that is the case
engagement_helper(History, Timeframe, TimeStep) when
                  length(History) > 0, length(Timeframe) > 0 ->
    First = hd(Timeframe),
    PrevS = find_previous(History, First, hd(History)),
    PrevIn = agent_util:snapshot_sum(indegrees, PrevS),
    PrevOut = agent_util:snapshot_sum(outdegrees, PrevS),
    % bit of a dirty hack but set the initial LastTs to
    % first S#snapshot.timestamp - TimeStep
    
    LastTs = util:sub_seconds_from_ts(First#snapshot.timestamp, TimeStep),

    process_snapshots(tl(Timeframe), First, {LastTs, PrevIn, PrevOut}, TimeStep, []);

% no timeframe or history to go on
engagement_helper(History, Timeframe, _Timestep) when
                  length(History) == 0; length(Timeframe) == 0 ->
    [].
    


%% process_snapshots
% Snapshots: chronological list of snapshots, to be processed
% S: current snapshot to be added
% {LastTimeStamp, LastSumIndegrees, LastSumOutdegrees}
% TimeStep: Check intervals in seconds
% [ {Ts, {in_diff, Integer}, {out_diff, Integer}} ]
% Results is in reverse order during processing, cons-ing newer ones


% end
process_snapshots([], S = #snapshot{}, {LastTs, _LastIn, _LastOut}, _TimeStep, Results) when
                   S#snapshot.timestamp =< LastTs ->
    % correct last ones Timestamp
    Tstr = util:timestamp_to_timestring(S#snapshot.timestamp),
    NewR = [setelement(1,hd(Results),Tstr) | tl(Results)],
    lists:reverse(NewR);

% loop
process_snapshots(Snapshots, S, {LastTs, LastIn, LastOut}, TimeStep, Results) ->
    CurrentIn = agent_util:snapshot_sum(indegrees, S),
    CurrentOut = agent_util:snapshot_sum(outdegrees, S),
    CurrentTs = util:add_seconds_to_ts(LastTs,TimeStep),
    CurrentTstr = util:timestamp_to_timestring(CurrentTs),
    NewResults = [{CurrentTstr,{in_diff, CurrentIn-LastIn},
                               {out_diff, CurrentOut-LastOut}} |
                  Results],
    {Next, RestSnapshots} = get_next(Snapshots, S,
                                     util:add_seconds_to_ts(CurrentTs,TimeStep)),

    process_snapshots(RestSnapshots, Next,
                      {CurrentTs, CurrentIn, CurrentOut},
                      TimeStep, NewResults).


% find the latest Snapshot with timestamp >= S.ts <= Ts
% return: {NewSnapshot, NewRest}
get_next([], S = #snapshot{}, _Ts) ->
    {S, []};

get_next([H = #snapshot{} | T], S = #snapshot{}, Ts) when 
         H#snapshot.timestamp > Ts ->

    {S, [H | T]};

get_next([H = #snapshot{} | T], _S = #snapshot{}, Ts) ->
    get_next(T, H, Ts).


% utility to find previous Snapshot in a full list
% used to set initial InDiff/OutDiff
find_previous([], _S, Latest) -> Latest;

find_previous([H = #snapshot{} | T], S = #snapshot{}, _Latest) when
              H#snapshot.timestamp < S#snapshot.timestamp ->
    find_previous(T, S, H);

find_previous([H = #snapshot{} | _T], S = #snapshot{}, Latest) when
              H#snapshot.timestamp >= S#snapshot.timestamp ->
    Latest.








