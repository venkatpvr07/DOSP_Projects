-module(gossip_main).
-import(neighbor,[findNeighbours/3]).
-export([begin_gossip/3,start_spawn/4,return_neighbors/3,supervisor/2,final_convergence/2,actor/3,child_actor/1,child_starter/1]).


begin_gossip(NumNodes, Topology, NumGossips) ->
  PidList = [],
  start_spawn(NumNodes,Topology,PidList,NumGossips ).

return_neighbors(0,PidList,Topology)->
  ok;
return_neighbors(Count,PidList,Topology) ->
%%  io:fwrite("\n In send Nei count:~w",[Count]),
  ProcessId = lists:nth(Count, PidList),
  Neighbor_List = findNeighbours(Topology,PidList,Count),
%%  io:fwrite("\n Nei List of ~w => ~w",[ProcessId,Neighbor_List]),
  ProcessId ! {Neighbor_List},
  return_neighbors(Count-1, PidList,Topology).

%%supervisor code
supervisor(PidList,Topology) ->
  {StartTime,_} = statistics(wall_clock),
%%  DeleteList=[],
%%  io:fwrite("start time is ~w\n",[StartTime]),
  return_neighbors(length(PidList),PidList,Topology),
%%  io:fwrite("\n send Nei ended"),
  FirstPid = lists:nth(rand:uniform(length(PidList)),PidList),
%%  io:fwrite("\nFirst PID invoked ~w",[FirstPid]),
  FirstPid ! {ok,gossip},
  register(final_convergence_name,spawn(gossip_main,final_convergence,[length(PidList),StartTime])).

final_convergence(0,StartTime) ->
  {EndTime,_} = statistics(wall_clock),
  io:fwrite("\n\nConvergence time  ~wms\n",[EndTime-StartTime]);
final_convergence(Count,StartTime)->
  receive
    dead ->
      ok
  end,
  final_convergence(Count-1,StartTime).


actor(0,NumGossips,Pid) ->
  exit(Pid,normal),
  io:fwrite("~w Process converged \n",[self()]),
  final_convergence_name ! dead,
  exit(normal);

actor(Count,NumGossips,Pid)->
  receive
    {Neighbor_List} ->
%%      io:fwrite("\nReceived Nei List, Count: ~w PID:~w",[Count,self()]),
      ChildPid =spawn(gossip_main,child_starter,[Neighbor_List]),
%%      io:fwrite("\nchild Process of ~w is ~w",[self(),ChildPid]),
      actor(Count-1,NumGossips,ChildPid);
    {ok,gossip} ->
%%      io:fwrite("\nok gossip received"),
      if
        Count == NumGossips ->
          Pid ! startgossip;
        true ->
          ok
      end
  end,
  actor(Count-1,NumGossips,Pid).

child_starter(Neighbor_List) ->
  receive
    startgossip ->
      child_actor(Neighbor_List)
  end.

child_actor(Neighbor_List) ->
%%  io:fwrite("Child actor Nei List ~w",[Neighbor_List]),
  ProcessId = lists:nth(rand:uniform(length(Neighbor_List)),Neighbor_List),
  ProcessId ! {ok,gossip} ,
  child_actor(Neighbor_List).

start_spawn(0,Topology,PidList,NumGossips) ->
%%  io:fwrite("\nSupervisor spawned"),
  register(supervisor_name,spawn(gossip_main,supervisor,[PidList,Topology]));

start_spawn(NumNodes,Topology, PidList, NumGossips ) ->
%%  io:fwrite("\n NumNodes :~w",[NumNodes]),
  Pid =0,
  ProcessId = spawn('gossip_main',actor,[NumGossips + 1,NumGossips,Pid]),
  PidListNew = lists:append(PidList,[ProcessId]),
%%  if
%%    NumNodes == 0 -> register(supervisor_name,spawn(gossip_main,supervisor,[PidList,Topology]));
%%    true -> start_spawn(NumNodes-1,Topology, PidListNew,NumGossips)
%%  end.
  start_spawn(NumNodes-1,Topology, PidListNew,NumGossips).