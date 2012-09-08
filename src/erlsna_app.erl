-module(erlsna_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, terminate/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erlsna_sup:start_link().

stop(_State) ->
    init:stop().

terminate(_Reason, _State, _Data) ->
    init:stop().
