-module(agent_sup).
-behaviour(supervisor).

-export([start_link/0, start_agent/1, stop_agent/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 60, 3600},
         [{an_agent,
          {agent, start_link, []},
          transient, 1000, worker, [agent]}
         ]}}.

start_agent(Agent) ->
    supervisor:start_child(?MODULE, [Agent]).

stop_agent(AgentPid) ->
    supervisor:terminate_child(?MODULE, AgentPid).
