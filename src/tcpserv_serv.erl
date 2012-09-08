%%% Inspired from LearnYouSomeErlang at
%%% http://learnyousomeerlang.com/static/erlang/processquest/apps/sockserv-1.0.1/src/sockserv_serv.erl

%%% Handles socket connections, used for accepting real-time TCP input

-module(tcpserv_serv).
-behaviour(gen_server).

-record(state, {status, count=0, socket}).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(SOCK(Msg), {tcp, _Port, Msg}).

% parse data list
parse_data(Data, []) -> {ok, erlang:now(), Data};
% Change: last item -> assume Timestamp
%parse_data(_Data, [_]) -> error;
parse_data(Data, [Ts]) -> {ok, util:timestring_to_timestamp(Ts), Data};
parse_data(Data, [Key, Value | DataListTail]) ->
    parse_data(orddict:store(list_to_atom(Key),Value,Data), DataListTail).
parse_data(DataList) -> parse_data(orddict:new(), DataList).

% TCP message parse

% agent
parse_tokens([agent, Id | TokenList]) ->
    % if Timestamp is not included in input, its injected in parse_data
    % ugly but simplifies return value and removes complexity at this point
    {ok, Ts, Data} = parse_data(TokenList),
    Agent = agent:make_agent(Id, Ts, Data),
    case gatekeeper:agent_exists(Id) of
        false -> 
            agent:create_agent(Agent),
            {created, Agent};
        true ->
            {error, agent_id_exists}
    end;

% delete_agent
parse_tokens([delete_agent, Id | _TokenList]) ->
    case gatekeeper:find_agent(Id) of
        {ok, AgentPid} ->
            {agent:delete_agent(AgentPid), {delete_agent, Id}};
        {error, not_found} ->
            {error, not_found}
    end;

% relation
parse_tokens([relation, SourceId, DestId, Value | Ts]) ->
    {ok, SourcePid} = gatekeeper:find_agent(SourceId),
    {ok, DestPid} = gatekeeper:find_agent(DestId),
    {V,_Rest} = string:to_integer(Value),
    case length(Ts) of
        0 ->
            agent:add_relation(SourcePid, DestPid, V),
            {ok, relation};
        1 ->
            agent:add_relation(SourcePid, DestPid, V, util:timestring_to_timestamp(hd(Ts))),
            {ok, relation_with_timestamp};
        _ ->
            {error, relation}
    end;


% adjust_relation
parse_tokens([adjust_relation, SourceId, DestId, Adjustment | Ts]) ->
    {ok, SourcePid} = gatekeeper:find_agent(SourceId),
    {ok, DestPid} = gatekeeper:find_agent(DestId),
    {A,_Rest} = string:to_integer(Adjustment),
    case length(Ts) of
        0 ->
            agent:adjust_relation(SourcePid, DestPid,A),
            {ok, adjust_relation};
        1 ->
            agent:adjust_relation(SourcePid, DestPid,A,util:timestring_to_timestamp(hd(Ts))),
            {ok, adjust_relation_with_timestamp};
        _ ->
            {error, adjust_relation}
    end;

% parse algorithm
parse_tokens([algorithm, AlgoName | Args]) ->
    Algorithm = list_to_atom(AlgoName),
    try algorithms:Algorithm(Args) of
        RetVal ->
        %    io:format("Input: ~p(~p)~n", [Algorithm, Args]),
            {Algorithm, RetVal}
    catch
        Type:Exception ->
            io:format("Algorithm Error: ~p(~p)~n", [Algorithm, Args]),
            io:format("~p:~p~n", [Type, Exception]),
            {Algorithm, error}
    end;

% parse pubsub
% TODO: just subscribe
parse_tokens([subscribe, AgentId, OperatorStr, ThresholdStr]) ->
    Pred = pubsub1:create_predicate(OperatorStr,ThresholdStr),
    case gatekeeper:find_agent(AgentId) of
        {ok, AgentPid} ->
          gen_server:cast(self(),{pubsub_subscribe, AgentPid, Pred}),
          {ok,subscribed};
    {error, not_found} ->
        {error, agent_not_found}
    end;
%%%%%%%%%%%

% anything else
parse_tokens(TokenList) ->
    io:format("Parse Error: ~p~n", [TokenList]),
    error.

% parse result to send back, limited
parse_result([algorithm, AlgoName | _Args], Result) ->
    parse_algorithm_result(list_to_atom(AlgoName), Result);

% Unparsed responses
parse_result(_, Result) ->
    Result.

% parse timestamped list of degree centralities
parse_algorithm_result(degree_centrality, {degree_centrality, [H|T]}) ->
    S = "~p indegree_centrality ~p outdegree_centrality ~p~n",
    % function for foldl
    F = fun({Ts, {indegree_centrality, I}, {outdegree_centrality, O}},Acc) ->
            Acc++io_lib:format(S,[Ts,I,O]) end,
    lists:foldl(F, [], [H|T]);

% parse timestamped engagement_analysis results
parse_algorithm_result(engagement_analysis, {engagement_analysis, [H|T]}) ->
    S = "~p in_diff ~p out_diff ~p~n",
    % function to foldl
    F = fun({Ts, {in_diff, I}, {out_diff, O}}, Acc) ->
            Acc++io_lib:format(S,[Ts,I,O]) end,
    lists:foldl(F, [], [H|T]);

% parse relative engagement results
parse_algorithm_result(relative_engagement,
                       {relative_engagement, {{network, NetworkList},
                                              {agent, AgentList}}}) ->
    S = "~p i_network ~p o_network ~p i_agent ~p o_agent ~p~n",
    F = fun({{Ts, {in_diff, I_N}, {out_diff, O_N}},
             {_Ts, {in_diff, I_A}, {out_diff, O_A}}},Acc) ->
        Acc++io_lib:format(S,[Ts,I_N,O_N,I_A,O_A]) end,
    lists:foldl(F, [], lists:zip(NetworkList, AgentList));
    


% Unparsed algorithms
parse_algorithm_result(_, Result) ->
    Result.
   
%% The socket is passed in from tcpserv_sup.
%% It's a listen socket, as started by gen_tcp:listen/2.
%%
%% In Erlang, a TCP socket must be started as a listening socket first.
%% The listening socket can then be used to listen for a connection,
%% meant to be accepted. To do so, use gen_tcp:accept/1-2, as it is done
%% later in this module.
%%
%% A single listen socket can be used by many processes, each accepting
%% a communication. When a communication is accepted with accept/1-2,
%% a new socket, called accept socket, is returned. This accept socket
%% is the one that may be used to communicate with a client.
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    %% properly seeding the process
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

%% Pubsub
handle_cast({pubsub_subscribe, AgentPid, Predicate}, State) ->
    events:pubsub_add(AgentPid,Predicate,self()),
    {noreply, State};

handle_cast({pubsub_publish, Notification}, State = #state{socket=Socket}) ->
    send(Socket, "~p", [Notification]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
%%%

%% Accepting a connection
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    %% this is the socket acceptance mentioned earlier
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    %% Remember that thou art dust, and to dust thou shalt return.
    %% We want to always keep a given number of children in this app.
    tcpserv_sup:start_socket(), % a new acceptor is born, praise the lord
    send(AcceptSocket, "Accepting Input...", []),
    {noreply, S#state{socket=AcceptSocket, status=accept}};

handle_cast({parse, [TokensH | TokensT]}, State = #state{socket=Socket}) ->
    TokenList = [list_to_atom(TokensH) | TokensT],
    io:format("Got Msg: ~p~n",[TokenList]),
    Result = parse_tokens(TokenList),
    %io:format("Result:  ~p~n", [Result]),
    ToSend = parse_result(TokenList,Result),
    case Result =:= ToSend of
        true ->
            send(Socket, "~p", [ToSend]);
        false ->
            gen_tcp:send(Socket, ToSend)
    end,
    inet:setopts(Socket, [{active, once}]),
    {noreply, State}.


%% The TCP client sends the string "quit". We close the connection.
handle_info(?SOCK("quit"++_), S) ->
    gen_tcp:close(S#state.socket),
    {stop, normal, S};


%% Handle TCP message to be parsed
handle_info(?SOCK(Str), S = #state{status=accept}) ->
    TokenList = line(Str),
    gen_server:cast(self(), {parse, TokenList}),
    NewCount = S#state.count + 1,
    io:format("~p: ~p~n", [NewCount, TokenList]),
    {noreply, S#state{count=NewCount}};


handle_info(?SOCK(E), S = #state{socket=Socket}) ->
    send(Socket, "Unexpected input: ~p~n", [E]),
    {noreply, S};
handle_info({tcp_closed, _}, S) ->
    {stop, normal, S};
handle_info(E, S) ->
    io:format("Unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, State) ->
    io:format("Parsed ~p messages. Terminated.~n", [State#state.count]),
    ok;

terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).

%% Send a message through a socket, then make it active again.
%% The difference between an active and a passive socket is that
%% an active socket will send incoming data as Erlang messages, while
%% passive sockets will require to be polled with gen_tcp:recv/2-3.

send(Socket, Str, Args) ->
    gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    inet:setopts(Socket, [{active, once}]),
    ok.

%% Let's get rid of the whitespace and ignore whatever's after.
%% makes it simpler to deal with telnet.
line(Str) ->
    string:tokens(Str, "\r\n ").
