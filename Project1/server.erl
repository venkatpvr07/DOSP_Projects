-module(server).
-export([start/1, supervisor/3, spawn_actor/2, actor/2, generate_random/2]).

start(K) ->
	io:format("\n Initial cpu time: ~w", [statistics(runtime)]),
	io:format("\n Initial real time: ~w", [statistics(wall_clock)]),
	Spawn_counter = 25,
	C = 1,
	register(reg_server, spawn(server, supervisor, [K, Spawn_counter, C])).
	
supervisor(K, Spawn_counter, C) ->
		io:format("\n Final cpu time: ~w", [statistics(runtime)]),
		io:format("\n Final real time: ~w", [statistics(wall_clock)]),
		 ok;
supervisor(K, Spawn_counter, C) ->
	if C == 1 ->
		spawn_actor(K, Spawn_counter);
	true ->
		unknown
	end,
	receive
		{Input_String, Hash, A_Pid} ->
			io:fwrite("\n ~s	~s", [Input_String, Hash]);
		{Input_String, Hash, B_Pid, fromClient} ->
			io:fwrite("\n ~s	~s", [Input_String, Hash]);
		{reg_client, Client_Machine, sendK, please, done} ->
			{reg_client, Client_Machine} ! {K, sent}
	end,

 supervisor(K, Spawn_counter, 0).
	
spawn_actor(K, 0) ->
	ok;

spawn_actor(K, Spawn_counter) ->
	spawn(server, actor, [K, reg_server]),
	spawn_actor(K, Spawn_counter - 1).

actor(K, reg_server) ->
	GLNAME = "vpamarthy;",
	RC = generate_random(rand:uniform(10), "abcdefghijklmnopqrstuvwxyz0123456789"),
	Input_String = GLNAME ++ RC,
	Hash = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,
	Input_String))]),
	P = string:slice(Hash,0,K),
	Q = string:copies("0",K),
	if P == Q ->
		{reg_server, node()} ! {Input_String, Hash, self()};
	true ->
		unknown
	end,
    actor(K, reg_server).
		
generate_random(Length, AllowedChars) ->
  MaxLength = length(AllowedChars),
  lists:foldl(
    fun(_, Acc) -> [lists:nth(crypto:rand_uniform(1, MaxLength), AllowedChars)] ++ Acc end,
    [], lists:seq(1, Length)
  ).