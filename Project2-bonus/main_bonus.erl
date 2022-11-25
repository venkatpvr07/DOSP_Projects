-module(main_bonus).
-import(gossip_bonus,[begin_gossip/4]).
-import(pushsum_bonus,[begin_pushsum/3]).
-export([start/5]).
start(NodeCount,Topology,Algorithm,NumGossips, Fault) ->
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
    Algorithm == gossip -> begin_gossip(NumNodes, Topology, NumGossips, Fault);
    true ->
      begin_pushsum(NumNodes, Topology, Fault)
  end.
