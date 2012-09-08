%%---------------------------------------------------------------------
%% Data Type: agent
%% where:
%%    id: string (default is undefined)
%%    pid: PID (default is undefind)
%%    created: timestamp(), see erlang:now/0 (default is undefined)
%%    indegrees, outdegrees, data: orddict() (default is orddict:new/0)
%%    history: list (default is [])
%%---------------------------------------------------------------------
-record(agent, {id, pid, created,
                         indegrees=orddict:new(),
                         outdegrees=orddict:new(),
                         data=orddict:new(),
                         history=[]}).

%%---------------------------------------------------------------------
%% Data Type: snapshot
%% where:
%%    timestamp: timestamp() (see erlang:now/0)
%%    note: string (default is undefined)
%%    indegrees, outdegrees, data: orddict() (default is undefined)
%%---------------------------------------------------------------------
-record(snapshot, {timestamp, note, indegrees, outdegrees, data}).
