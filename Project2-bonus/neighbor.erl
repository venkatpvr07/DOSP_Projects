-module('neighbor').
-export([findNeighbours/3,remove_ok/2,random_picker/4]).

findNeighbours(TopologyName, PidList,Index) ->
  case  TopologyName of
    full  ->
        lists:delete(self(),PidList);
    line ->
      N = length(PidList),
      Neighbor_List = [
        if
          Index-1 > 0->
            lists:nth(Index-1,PidList);
          true->
            ok
        end,
        if
          Index +1 =< N->
            lists:nth(Index+1,PidList);
          true->
            ok
        end
      ],
      remove_ok(Neighbor_List,length(Neighbor_List));
    '2d'->
      N = trunc(math:sqrt(length(PidList))),
      Neighbor_List = [
        if
          ((Index-1) rem N) == 0 ->%%Left neighbour
            ok;
          true -> lists:nth(Index-1,PidList)

        end,
        if
          (Index rem N) == 0->%%Right neighbour
            ok;
          true -> lists:nth(Index+1,PidList)
        end,
        if
          Index - N > 0->%%Top neighbour
            lists:nth(Index-N,PidList);
          true -> ok
        end,
        if
          Index + N =< N*N ->%%Bottom neighbour
            lists:nth(Index+N,PidList);
          true -> ok
        end
      ],
      remove_ok(Neighbor_List,length(Neighbor_List));
    imp3d->
      N = trunc(math:sqrt(length(PidList))),
      Neighbor_List = [
        if
          (Index - N > 0) and (((Index-1) rem N) /= 0 ) == true ->   %%Top left neighbour
            lists:nth(Index-N-1,PidList);
          true -> ok
        end,
        if
          Index - N > 0->%%Top neighbour
            lists:nth(Index-N,PidList);
          true -> ok
        end,
        if
          (Index - N > 0) and ((Index rem N) /= 0 ) == true ->   %%Top Right neighbour
            lists:nth(Index-N+1,PidList);
          true -> ok
        end,
        if
          ((Index-1) rem N) == 0 ->%%Left neighbour
            ok;
          true -> lists:nth(Index-1,PidList)
        end,
        if
          (Index rem N) == 0-> %%Right neighbour
            ok;
          true -> lists:nth(Index+1,PidList)
        end,
        if
          (Index + N =< (N*N)) and (((Index-1) rem N) /= 0 ) == true ->   %%Bottom left neighbour
            lists:nth(Index+N-1,PidList);
          true -> ok
        end,
        if
          Index + N =< N*N ->%%Bottom neighbour
            lists:nth(Index+N,PidList);
          true -> ok
        end,
        if
          (Index + N =< (N*N)) and ((Index rem N) /= 0 ) == true ->   %%Bottom left neighbour
            lists:nth(Index+N+1,PidList);
          true -> ok
        end
      ],
      NewNeiList = remove_ok(Neighbor_List,length(Neighbor_List)),
      NewProcList = lists:delete(lists:nth(Index,PidList),PidList),
      if(length(NewNeiList) == length(NewProcList)) -> NewNeiList;
        true ->
          lists:append(NewNeiList,[random_picker(length(NewProcList),NewNeiList,NewProcList,[])])
      end
      end.

remove_ok(Neighbor_List,0)->
  Neighbor_List;

remove_ok(Neighbor_List,Length) ->
  NewList = lists:delete(ok,Neighbor_List),
  remove_ok(NewList,Length-1).

random_picker(0,Neighbor_List,PidList,TempNonNeighborList) ->
%%  io:fwrite("\nTemp Non Nei List : ~w",[TempNonNeighborList]),
  lists:nth(rand:uniform(length(TempNonNeighborList)),TempNonNeighborList);


random_picker(Count,Neighbor_List,PidList,NonNeighborList) ->
  Member = lists:nth(Count,PidList),
  Is_member = lists:member((Member),Neighbor_List),
  if Is_member == false  ->
    TempNonNeighborList = lists:append(NonNeighborList,[Member]);
  true ->
    TempNonNeighborList = NonNeighborList
  end,
    random_picker(Count-1,Neighbor_List,PidList,TempNonNeighborList).
