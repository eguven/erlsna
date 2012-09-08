-module(events).
-export([start_link/0, new_relation/2, new_relation/3, delayed_snapshot/3,
         pubsub_add/3]).

start_link() ->
    {ok, Pid} = gen_event:start_link(),
    register(events,Pid),
    gen_event:add_handler(Pid, eventkeeper, []),
    gen_event:add_handler(Pid, pubsub1, [[]]),
    {ok, Pid}.

new_relation(PidA, PidB) ->
    new_relation(PidA, PidB, erlang:now()).

new_relation(PidA, PidB, Timestamp) ->
    gen_event:notify(whereis(events), {new_relation, PidA, PidB, Timestamp}),
    % fire PubSub event
    gen_event:notify(whereis(events), {check_pubsub}).

% AgentPid can be a Pid or a list of Pids
delayed_snapshot(AgentPid, MsDelay, Note) ->
    gen_event:notify(whereis(events), {delayed_snapshot, AgentPid, MsDelay, Note}).

% Dynamic / PubSub add, preliminary

%ets:new(updates_ets, [set, public, named_table]),
%ets:insert(updates_ets, {running, false}),

% {AgentPid,Predicate,SubscriberPid}

pubsub_add(AgentPid,Predicate,SubscriberPid) ->
  gen_event:notify(whereis(events), {add_agent,AgentPid,Predicate,SubscriberPid}).
%  gen_event:add_handler(whereis(events), pubsub1, [AgentPid,Predicate,SubscriberPid]).
