
-module(erlsna_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % Gatekeeper process, keeps record
    Gatekeeper = ?CHILD(gatekeeper, worker),
    % TcpServer socket supervisor, spawns sockets to accept connections
    TcpServer = ?CHILD(tcpserv_sup, supervisor),
    % Agent supervisor
    AgentSup = ?CHILD(agent_sup, supervisor),
    % Event Manager, module: events
    EventMan = {events,
                {events, start_link, []},
                permanent, 5000, worker, dynamic},
    {ok, { {one_for_one, 5, 10}, [Gatekeeper, TcpServer, AgentSup, EventMan]} }.

