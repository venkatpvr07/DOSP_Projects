-module(bonus_main).
-import(bonus_fingertablemodule,[ft_calculation/5,ft_data/4,ft_peer_pass/3,ft_send/2]).
-import(bonus_hopmodule,[peer_wait/1,allHops/0]).
-export([start/3, begin_net/2, task_listen/2, actor/4,get_succ/4,closest_peer/3,next_peer/2,prev_peer/2]).

start(NodeCount, NumRequest, FailureNodes) ->
    register(supervisor, spawn(bonus_main, begin_net, [NodeCount-FailureNodes, NumRequest])).

begin_net(NodeCount, NumRequest) ->
    Bit_M = get_bit_m(NodeCount),
    [NetNodes, NetState] = gen_peers([], round(math:pow(2, Bit_M)), Bit_M, NodeCount, dict:new()),

    ft_send(NetState,Bit_M),
    ping_and_destroy(NetNodes, NodeCount, NumRequest, Bit_M, NetState).

get_bit_m(NodeCount) ->
    trunc(math:ceil(math:log2(NodeCount))).

find_forward_length(K, K, _, Length) ->
    Length;
find_forward_length(K, PeerId, Bit_M, Length) ->
    find_forward_length(K, (PeerId + 1) rem trunc(math:pow(2, Bit_M)), Bit_M, Length + 1).

rand_peer(Id, []) -> Id;
rand_peer(_, InNodes) -> lists:nth(rand:uniform(length(InNodes)), InNodes).


add_chord(NetNodes, AllNodes, Bit_M, NetState) ->
    RemPeers = lists:seq(0, AllNodes - 1, 1) -- NetNodes,
    Peer = lists:nth(rand:uniform(length(RemPeers)), RemPeers),
    ProcessId = spawn(bonus_main, actor, [Peer, Bit_M, NetNodes, dict:new()]),
    [Peer, dict:store(Peer, ProcessId, NetState)].


find_closest(_, [], NodeMin, _, _) ->
    NodeMin;
find_closest(K, FingerTableIds, NodeMin, MinimumValue, Status) ->
    [First_Id| Rem_Ids] = FingerTableIds,
    Length = find_forward_length(K, First_Id, dict:fetch(m, Status), 0),
    if
        Length < MinimumValue ->
            find_closest(K, Rem_Ids, First_Id, Length, Status);
        true ->
            find_closest(K, Rem_Ids, NodeMin, MinimumValue, Status)
    end.

closest_peer(K, FingerTableIds, Status) ->
    case lists:member(K, FingerTableIds) of
        true -> K;
        _ -> find_closest(K, FingerTableIds, -1, 10000000, Status)
    end.


is_in_range(Begin, End, K, Bit_M) ->
    if
        Begin < End ->
            (Begin =< K) and (K =< End);
        trunc(Begin) == trunc(End) ->
            trunc(K) == trunc(Begin);
        Begin > End ->
            ((K >= 0) and (K =< End)) or ((K >= Begin) and (K < trunc(math:pow(2, Bit_M))))
    end.


prev_peer(Id, PeerStatus) ->
    case
        is_in_range(dict:fetch(id, PeerStatus) + 1, dict:fetch(id, dict:fetch(succ, PeerStatus)), Id, dict:fetch(m, PeerStatus)) of
            true -> PeerStatus;
            _ -> prev_peer(Id, closest_preceding_finger(Id, PeerStatus, dict:fetch(m, PeerStatus)))
    end.

next_peer(Id, PeerStatus) ->
    PredStatus = prev_peer(Id, PeerStatus),
    dict:fetch(succ, PredStatus).


closest_preceding_finger(_, PeerStatus, 0) -> PeerStatus;
closest_preceding_finger(Id, PeerStatus, Bit_M) ->
    Finger_M = lists:nth(Bit_M, dict:fetch(finger_table, PeerStatus)),

    case is_in_range(dict:fetch(id, PeerStatus), Id, dict:fetch(actor ,Finger_M), dict:fetch(m, PeerStatus)) of
        true ->

            dict:fetch(processid ,Finger_M) ! {status, self()},
            receive
                {statusquery, FPeerStatus} ->
                    FPeerStatus
            end,
            FPeerStatus;

        _ -> closest_preceding_finger(Id, PeerStatus, Bit_M - 1)
    end.

gen_peers(NetNodes, _, _, 0, NetState) ->
    [NetNodes, NetState];
gen_peers(NetNodes, AllNodes, Bit_M, NodeCount, NetState) ->
    [Peer, NewNetworkState] = add_chord(NetNodes, AllNodes,  Bit_M, NetState),
    gen_peers(lists:append(NetNodes, [Peer]), AllNodes, Bit_M, NodeCount - 1, NewNetworkState).

actor(Peer, Bit_M, NetNodes, _NodeState) ->
    %io:format("Node is spawned with hash ~p",[Peer]),
    F_Table = lists:duplicate(Bit_M, rand_peer(Peer, NetNodes)),
    UpdatedPeerStatus = dict:from_list([{id, Peer}, {predecessor, nil}, {finger_table, F_Table}, {next, 0}, {m, Bit_M}]),
    peer_wait(UpdatedPeerStatus).




get_succ(Peer, NetState, A,  Bit_M) ->
    case dict:find((Peer + A) rem trunc(math:pow(2, Bit_M)), NetState) of
        error ->
             get_succ(Peer, NetState, A + 1, Bit_M);
        _ -> (Peer + A) rem trunc(math:pow(2, Bit_M))
    end.



get_node_pid(Peer, NetState) ->
    case dict:find(Peer, NetState) of
        error -> nil;
        _ -> dict:fetch(Peer, NetState)
    end.

ping_one_node(_, [], _) ->
    ok;
ping_one_node(K, NetNodes, NetState) ->
    [First_Id | Rem_Ids] = NetNodes,
    ProcessId = get_node_pid(First_Id, NetState),
    ProcessId ! {lookup, First_Id, K, 0, self()},
    ping_one_node(K, Rem_Ids, NetState).



destroy_peers([], _) ->
    ok;
destroy_peers(NetNodes, NetState) ->
    [First_Id | Rem_Ids] = NetNodes,
    get_node_pid(First_Id, NetState) ! {kill},
    destroy_peers(Rem_Ids, NetState).

ping_nodes(_, 0, _, _) ->
    ok;
ping_nodes(NetNodes, NumRequest, Bit_M, NetState) ->
    timer:sleep(1000),
    K = lists:nth(rand:uniform(length(NetNodes)), NetNodes),
    ping_one_node(K, NetNodes, NetState),
    ping_nodes(NetNodes, NumRequest - 1, Bit_M, NetState).


ping_and_destroy(NetNodes, NodeCount, NumRequest, Bit_M, NetState) ->
    register(taskcompletionmonitor, spawn(bonus_main, task_listen, [NodeCount * NumRequest, 0])),

    ping_nodes(NetNodes, NumRequest, Bit_M, NetState),

    TotalHops = allHops(),
    AllowedHops = math:log2(NodeCount),
    io:format("~n Average Hops = ~p   TotalHops = ~p ~n", [TotalHops/(NodeCount * NumRequest), TotalHops]),
    io:format("~n Allowed Log Hops = ~p ~n", [AllowedHops]),
    destroy_peers(NetNodes, NetState).


task_listen(0, HopsCount) ->
    supervisor ! {totalhops, HopsCount};

task_listen(NumRequests, HopsCount) ->
    receive
        {completed, _Pid, HopsCountForTask, _Key} ->
            % io:format("received completion from ~p, Number of Hops ~p, For K ~p", [ProcessId, HopsCountForTask, K]),
            task_listen(NumRequests - 1, HopsCount + HopsCountForTask)
    end.