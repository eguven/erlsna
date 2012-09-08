%%% Inspired (mostly copy/pasted :)) from LearnYouSomeErlang at
%%% http://learnyousomeerlang.com/static/erlang/processquest/apps/sockserv-1.0.1/src/sockserv_serv.erl

-module(tcpserv_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, start_socket/0, import_hook/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(tcpserv_port),
    % reuseaddr true to counter the joys of CLOSE_WAIT
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}, {reuseaddr,true}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
         [{socket,
          {tcpserv_serv, start_link, [ListenSocket]},
          temporary, 1000, worker, [tcpserv_serv]}
         ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

%% start listeneres
empty_listeners() ->
    [start_socket() || _ <- lists:seq(1,20)],
    ok.

import_hook() -> ok.
