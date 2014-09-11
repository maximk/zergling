-module(zergling_app).

-behaviour(application).

-export([start/0]).

-export([timestamp/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowlib),
	application:start(cowboy),
	application:start(zergling).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	TsAppStarted = timestamp(),

	%% shutdown after timeout if client does not connect
	case init:get_argument(shutdown_after) of
	{ok,[[TStr]]} ->
		Timeout = list_to_integer(TStr) *1000,
		spawn(fun() ->
			receive after Timeout -> ok end,
			init:stop()
		end);
	error ->
		ok
	end,
	
	%%
	%% erlang:statistics(wall_clock) is called in the beginning of init:boot().
	%%
	{SinceEntryMs,SinceInitBootMs} = erlang:statistics(wall_clock),

	TsLingStarted = TsAppStarted - SinceEntryMs / 1000,
	TsBootStarted = TsAppStarted - SinceInitBootMs / 1000,

 	Dispatch = cowboy_router:compile([
		{'_',[
			{"/",toppage_handler,[]}
		]}
	]),

	{ok,_} = cowboy:start_http(www, 8,
		[{port,8000}],
		[{env,[{dispatch,Dispatch}]}]),

	TsCowboyStarted = timestamp(),

	case init:get_argument(notify) of
	{ok,[[SpawnerSpot]]} ->
		notify_spawner(SpawnerSpot);
	_ ->
		ok
	end,

	TsSpawnerNotified = timestamp(),

	OtherVars = [{ts_ling_started,TsLingStarted},
				 {ts_boot_started,TsBootStarted},
				 {ts_app_started,TsAppStarted},
				 {ts_cowboy_started,TsCowboyStarted},
				 {ts_spawner_notified,TsSpawnerNotified}]

		++ config_args(),

	Pid = spawn(fun() ->
		keeper(OtherVars)
	end),
	register(keeper, Pid),

	zergling_sup:start_link().

stop(_State) ->
    ok.

%%------------------------------------------------------------------------------

keeper(Vars) ->
	receive
	{get,From} ->
		From ! {vars,Vars},
		keeper(Vars)
	end.

notify_spawner(SpawnerSpec) ->
	[SpawnerHost,SpawnerPort] = string:tokens(SpawnerSpec, ":"),
	{ok,IpAddr} = inet_parse_address(SpawnerHost),
	Port = list_to_integer(SpawnerPort),
	{ok,Sock} = gen_tcp:connect(IpAddr, Port, [{active,false}]),
	DomName = ling:domain_name(),
	Packet = list_to_binary(["POST /ready/",DomName," HTTP/1.0\r\n\r\n"]),
	ok  = gen_tcp:send(Sock, Packet),
	{ok,_Data} = gen_tcp:recv(Sock, 0),
	ok = gen_tcp:close(Sock).

inet_parse_address(Host) ->
	[A,B,C,D] = string:tokens(Host, "."),
	{ok,{list_to_integer(A),
		 list_to_integer(B),
		 list_to_integer(C),
		 list_to_integer(D)}}.

timestamp() ->
	{Mega,Secs,Micro} = now(),
	Mega *1000000.0 + Secs + Micro / 1000000.0.

config_args() ->
	{ok,[[TRR]]} = init:get_argument(ts_req_received),
	[{ts_req_received,list_to_float(TRR)}].

%%EOF
