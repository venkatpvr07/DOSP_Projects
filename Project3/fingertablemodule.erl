-module(fingertablemodule).
-import(main,[get_succ/4]).
-export([ft_calculation/5,ft_data/4,ft_peer_pass/3,ft_send/2]).



ft_send(NetState,Bit_M) ->
    FT = ft_data(NetState, dict:to_list(NetState), dict:new(),Bit_M),
    % io:format("~n~p~n", [FT]),
    ft_peer_pass(dict:fetch_keys(FT), NetState, FT).

ft_calculation(_, _, Bit_M, Bit_M,F_List) ->
    F_List;
ft_calculation(Node, NetState, Bit_M, A, F_List) ->
    Peer = element(1, Node),
    Succ_A = get_succ(Peer, NetState, trunc(math:pow(2, A)), Bit_M),
    ft_calculation(Node, NetState, Bit_M, A + 1, F_List ++ [{Succ_A, dict:fetch(Succ_A, NetState)}] ).

ft_peer_pass([], _, _) ->
    ok;
ft_peer_pass(SendPeers, NetState, FT) ->
    [First_Id|Re_Ids] = SendPeers,
    Pid = dict:fetch(First_Id ,NetState),
    Pid ! {fix_fingers, dict:from_list(dict:fetch(First_Id, FT))},
    ft_peer_pass(Re_Ids, NetState, FT).

ft_data(_, [], FingerTableDictionary,_) ->
    FingerTableDictionary;

ft_data(NetState, NetList, FingerTableDictionary,Bit_M) ->
    [First_Id | Re_Ids] = NetList,
    FT = ft_calculation(First_Id, NetState,Bit_M, 0,[]),
    ft_data(NetState, Re_Ids, dict:store(element(1, First_Id), FT, FingerTableDictionary), Bit_M).


