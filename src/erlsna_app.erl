-module(erlsna_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, terminate/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    crypto_app_check(),
    erlsna_sup:start_link().

stop(_State) ->
    init:stop().

terminate(_Reason, _State, _Data) ->
    init:stop().

%% ===================================================================
%% Applications required to be running prior to start
%% ===================================================================

crypto_app_check() ->
    Apps = orddict:fetch(started,application:info()),
    case orddict:find(crypto,orddict:from_list(Apps)) of
        {ok,Type} ->
            io:format("Crypto running: ~p~n",[Type]);
        error ->
            application:start(crypto),
            io:format("Crypto started~n",[])
    end.
