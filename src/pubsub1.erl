-module(pubsub1).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).
-export([create_predicate/2]).

%-record(subscription, {agentPid,algorithm,retvalExtractor,predicate,subscriberPid}).

%make_subscription(AgentPid,AlgorithmAtom,RetvalExtractor,Predicate,SubscriberPid) ->
%    #subscription{agentPid=AgentPid,algorithm=AlgorithmAtom,
%                  retvalExtractor=RetvalExtractor,predicate=Predicate,
%                  subscriberPid=SubscriberPid}.

% Agents: {AgentPid, Predicate,SubscriberPid} list
% SubscriberPid is the Pid of tcpserv_serv process with socket connected to the subscriber
% TODO: move to records
init([Agents]) ->
    {ok,Agents}.

handle_event({add_agent, AgentPid, Predicate, SubscriberPid}, Agents) ->
    % remove old subscription for the agent if any
    %AgentsTmp = lists:keydelete(AgentPid,1,Agents),
    AgentsTmp = lists:delete({AgentPid,Predicate,SubscriberPid},Agents),
    % add new subscription
    NewAgents = [{AgentPid,Predicate,SubscriberPid}|AgentsTmp],
    {ok, NewAgents};

handle_event({check_pubsub}, Agents) ->
    % TODO: clean up dead subscribers sometime
    [check_helper(APid,Predicate,SubscriberPid) || {APid,Predicate,SubscriberPid} <- Agents],
    {ok, Agents};

handle_event(_, State) ->
    {ok, State}.

check_helper(AgentPid, Predicate, SubscriberPid) ->
  io:format("Check helper for ~p with subscriber ~p~n",[AgentPid,SubscriberPid]),
  case check_agent(AgentPid,Predicate) of
    {true, Val} -> %tcpserv_serv:subscriber_notify(element(3,State),Val),
                   io:format("~p Publish Entered~n",[AgentPid]),
                   gen_server:cast(SubscriberPid,{pubsub_publish,Val}),
                   true;
    {false, _Val} -> false
  end.

check_agent(AgentPid, Predicate) ->
  {_Tag,Val} = algorithms:calc_closeness_centrality(AgentPid),
  % TODO: make use of history
  case agent:agent_info(AgentPid,closeness_centrality) of
    {error,not_found} ->
      agent:update_data(AgentPid,closeness_centrality,Val),
      agent:update_history(AgentPid,"ClosenessCentrality"),
      {false,Val};
    Value ->
      % call predicate with current,previous
      RetVal = Predicate(Val,Value),
      agent:update_data(AgentPid,closeness_centrality,Val),
      agent:update_history(AgentPid,"ClosenessCentrality"),
      % eg. {true,0.3}
      {RetVal,Val}
  end.

%% Previous is necessary to catch if it exceeded the threshold
create_predicate(OperatorStr,ThresholdStr) ->
  {Threshold,_Rest} = string:to_float(ThresholdStr),
  case OperatorStr of
    ">" ->
      fun(Current,Previous) -> (Current > Threshold) and (Previous =< Threshold) end;
    "<" ->
      fun(Current,Previous) -> (Current < Threshold) and (Previous >= Threshold) end;
    "=" -> 
      fun(Current,_Previous) -> Current == Threshold end;
    "*" ->
      fun(_Current,_Previous) -> true end
  end.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    %io:format("~n** terminated~nReason: ~p~nState:~p~n",[Reason,State]),
    ok.
