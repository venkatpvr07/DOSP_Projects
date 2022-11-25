-module(hopmodule).
-import(fingertablemodule,[ft_calculation/5,ft_data/4,ft_peer_pass/3,ft_send/2]).
-import(main,[closest_peer/3,next_peer/2]).
-export([peer_wait/1,allHops/0]).

allHops() ->
  receive
    {totalhops, CountHops} ->
      CountHops
  end.

peer_wait(PeerState) ->
    Peer = dict:fetch(id, PeerState),
    receive
            
        {lookup, Identifier, K, CountHops, _ProcessId} ->

                NodeVal = closest_peer(K, dict:fetch_keys(dict:fetch(finger_table ,PeerState)), PeerState),
                UpdatedState = PeerState,
                %io:format("Lookup::: ~p  For K ~p  ClosestNode ~p ~n", [Peer, K, NodeVal]),
                if 
                    
                    (Peer == K) -> 
                        taskcompletionmonitor ! {completed, Peer, CountHops, K};
                    (NodeVal == K) and (Peer =/= K) -> 
                        taskcompletionmonitor ! {completed, Peer, CountHops, K};
                    
                    true ->
                        dict:fetch(NodeVal, dict:fetch(finger_table, PeerState)) ! {lookup, Identifier, K, CountHops + 1, self()}
                end
                ;
        {kill} ->
            UpdatedState = PeerState,
            exit("received exit");
        {state, ProcessId} -> ProcessId ! PeerState,
                        UpdatedState = PeerState;
        {get_successor, Identifier, ProcessId} ->
                        FoundSucc = next_peer(Identifier, PeerState),
                        UpdatedState = PeerState,
                        {ProcessId} ! {get_successor_reply, FoundSucc};

        
        {fix_fingers, FingerTable} ->
            UpdatedState = dict:store(finger_table, FingerTable, PeerState)
    end, 
    peer_wait(UpdatedState).

