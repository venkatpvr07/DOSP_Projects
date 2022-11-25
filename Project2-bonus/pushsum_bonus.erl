-module(pushsum_bonus).
-import('neighbor',[findNeighbours/3,delete/2]).
-export([begin_pushsum/3,start_spawn/4,return_neighbors/3,supervisor/3,actor/5,child_starter/1,child_actor/3,final_convergence/2, induce_fault/2]).


begin_pushsum(NumNodes, Topology, Fault) ->
  PidList = [],
  start_spawn(NumNodes,Topology,PidList, Fault).

induce_fault(PidList,0)->
  io:fwrite("\nAlive Nodes ~w",[PidList]),
  PidList;
induce_fault(PidList,FaultCount) ->
  ProcessToKill = lists:nth(rand:uniform(length(PidList)),PidList),
  NewList = lists:delete(ProcessToKill,PidList),
  ProcessToKill  ! {ok,killChild,suicide},
  induce_fault(NewList,FaultCount-1).

%%supervisor code
supervisor(PidList,Topology,Fault) ->
  {StartTime,_} = statistics(wall_clock),
%%  DeleteList=[],
%%  io:fwrite("\nstart time is ~w",[StartTime]),
  return_neighbors(length(PidList),PidList,Topology),
  io:fwrite("\nALl Nodes: ~w",[PidList]),
  io:fwrite("\n Supervisor Node ~w",[self()]),
  NewPidList = induce_fault(PidList,Fault),
%%  io:fwrite("\n Proc list : ~w\n",[PidList]),
%%  io:fwrite("\n send Nei ended"),
  FirstProcessId = lists:nth(rand:uniform(length(PidList)),PidList),
%%  io:fwrite("\nFirst PID invoked ~w",[FirstProcessId]),
  FirstProcessId ! {0,0},
  register(final_convergence_name,spawn(pushsum_bonus,final_convergence,[length(PidList),StartTime])).

return_neighbors(0,PidList,Topology)->
  ok;
return_neighbors(Count,PidList,Topology) ->
%%  io:fwrite("\n In send Nei count:~w",[Count]),
  ProcessId = lists:nth(Count, PidList),
  Neighbor_List = findNeighbours(Topology,PidList,Count),
%%  io:fwrite("\n Nei List of ~w => ~w",[ProcessId,Neighbor_List]),
  ProcessId ! {Neighbor_List},
  return_neighbors(Count-1, PidList,Topology).

final_convergence(0,StartTime) ->
  {EndTime,_} = statistics(wall_clock),
  io:fwrite("\n Convergence time is ~wms\n",[EndTime-StartTime]);
final_convergence(Count,StartTime)->
  receive
    dead ->
      ok;
    {ok,count} ->
      io:fwrite("Total number of processes not converged are ~w",[Count])
  end,
  final_convergence(Count-1,StartTime).


actor(S,W,Pid,0,L) ->
  exit(Pid, normal),
  io:fwrite("\nProcess Converged ~w",[self()]),
  final_convergence_name ! dead,
  exit(normal);
actor(S,W,Pid,RoundCount,L)->
  receive
    {Neighbor_List} ->
%%      io:fwrite("\nReceived Nei List, Count: ~w PID:~w",[Count,self()]),
      ChildProcessId =spawn(pushsum_bonus,child_starter,[Neighbor_List]),
      actor(S,W,ChildProcessId,3,Neighbor_List);
    {Sum,Weight} ->
%%        io:fwrite("\nReached new values"),
      exit(Pid,normal),
%%        io:fwrite("\nRemoved old process"),
      NewChildPid = spawn(pushsum_bonus,child_starter,[L]),
      NewS = (S+Sum)/2,
      NewW = (W+Weight)/2,
      Diff = abs((NewS/NewW)-(S/W)),
      Change = math:pow(10,-10),
      if
        Diff =< Change ->
          NewChildPid ! {initiatepushsum,NewS,NewW},
          actor(NewS,NewW,NewChildPid,RoundCount-1,L);
        true ->
          actor(NewS,NewW,NewChildPid,3,L)
%%          if
%%            RoundCount < 3 ->
%%              NewChildPid ! {initiatepushsum,NewS,NewW},
%%              actor(NewS,NewW,NewChildPid,RoundCount+1,L) ;
%%            true ->
%%              NewChildPid ! {initiatepushsum,NewS,NewW},
%%              actor(NewS,NewW,NewChildPid,RoundCount,L)
%%          end
      end;
      {ok,killChild,suicide} ->
      exit(Pid,normal),
      exit(normal)
  end.

child_starter(Neighbor_List) ->
  receive
    {initiatepushsum,NewS,NewW} ->
%%      io:fwrite("\nReached child_starter"),
      child_actor(Neighbor_List,NewS,NewW)
  end,
  child_starter(Neighbor_List).

child_actor(Neighbor_List,S,W) ->
%%  io:fwrite("\nReached child_actor"),
  ProcessId = lists:nth(rand:uniform(length(Neighbor_List)),Neighbor_List),
  ProcessId ! {S/2,W/2},
  child_actor(Neighbor_List,S,W).

start_spawn(0, Topology, PidList,Fault) ->
%%  io:fwrite("\nSupervisor spawned"),
  register(supervisor_name,spawn(pushsum_bonus,supervisor,[PidList,Topology,Fault]));

start_spawn(NumNodes, Topology, PidList,Fault) ->
%%  io:fwrite("\nActors spawned"),
  Pid =0,
  L=[],
  ProcessId = spawn(pushsum_bonus,actor,[NumNodes,1,Pid,3,L]),
  ProcList_temp = lists:append(PidList,[ProcessId]),
  start_spawn(NumNodes-1, Topology, ProcList_temp,Fault).
