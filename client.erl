-module(client).
-export([start/1, client_supervisor/2, client_spawn_actor/3, client_actor/3, generate_random/2]).

start(IP) ->
	{ok, H} = inet:gethostbyaddr(IP),
	List=tuple_to_list(H),
	Host = lists:nth(2,List),
	Server_Machine = list_to_atom("server@"++Host),
	Spawn_counter = 25,
	register(reg_client, spawn(client, client_supervisor, [Spawn_counter, Server_Machine])).
	
client_supervisor(Spawn_counter, Server_Machine) ->
	{reg_server, Server_Machine} ! {reg_client, node(), sendK, please, done},
	receive
		{K, sent} -> client_spawn_actor(K, Spawn_counter, Server_Machine)			
	end.
	
client_spawn_actor(K, 0, Server_Machine) ->
	ok;

client_spawn_actor(K, Spawn_counter, Server_Machine) ->
	spawn(client, client_actor, [K, reg_server, Server_Machine]),
	client_spawn_actor(K, Spawn_counter - 1, Server_Machine).

client_actor(K, SReg, Server_Machine) ->
	GLNAME = "vpamarthy;",
	RC = generate_random(rand:uniform(10), "abcdefghijklmnopqrstuvwxyz0123456789"),
	Input_String = GLNAME ++ RC,
	Hash = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,
	Input_String))]),
	P = string:slice(Hash,0,K),
	Q = string:copies("0",K),
	if P == Q ->
		{SReg, Server_Machine } ! {Input_String, Hash, self(), fromClient};
	true ->
		unknown
	end,
    client_actor(K, reg_server, Server_Machine).
		
generate_random(Length, AllowedChars) ->
  MaxLength = length(AllowedChars),
  lists:foldl(
    fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), AllowedChars)] ++ Acc end,
    [], lists:seq(1, Length)
  ).