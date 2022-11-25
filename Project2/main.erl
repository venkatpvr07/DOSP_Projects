-module(main).
-import(gossip_main,[begin_gossip/3]).
-import(pushsum_main,[begin_pushsum/2]).
-export([start/4]).
start(NodeCount,Topology,Algorithm,NumGossips) ->
%%  compile:compile()
%%  c(topology),
%%  c(gossip),
%%  N = string:to_integer(lists:nth(1, Input)),
%%  Topology = lists:nth(2, Input),
  if
    ((Topology == '2d') or (Topology == imp3d )) == true->
      NumNodes = trunc(math:pow(round(math:sqrt(NodeCount)),2));
%%    Topology == "imp2d" ->
%%      NumNodes = trunc(math:pow(round(math:sqrt(NumNodes)),2));
    true ->
      NumNodes = NodeCount
  end,
%%  Algorithm = lists:nth(3, Input),
%%  NumGossips = string:to_integer(lists:nth(4, Input)),
  if
    Algorithm == gossip -> begin_gossip(NumNodes, Topology, NumGossips);
    true ->
      begin_pushsum(NumNodes, Topology)
  end.
