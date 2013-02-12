-module(zergling_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowboy),
	application:start(zergling).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

 	Dispatch = cowboy_router:compile([
		{'_',[
			{"/",toppage_handler,[]}
		]}
	]),

	{ok,_} = cowboy:start_http(spawner, 8,
		[{port,8000}],
		[{env,[{dispatch,Dispatch}]}]),

	case init:get_argument(notify) of
	{ok,[[SpawnerSpot]]} ->
		notify_spawner(SpawnerSpot);
	_ ->
		ok
	end,

	zergling_sup:start_link().

stop(_State) ->
    ok.

%%------------------------------------------------------------------------------

notify_spawner(SpawnerSpec) ->
	[SpawnerHost,SpawnerPort] = string:tokens(SpawnerSpec, ":"),
	{ok,IpAddr} = inet_parse_address(SpawnerHost),
	Port = list_to_integer(SpawnerPort),
	{ok,Sock} = gen_tcp:connect(IpAddr, Port, [{active,false}]),
	Packet = <<"POST /ready HTTP/1.0\r\n\r\n">>,
	ok  = gen_tcp:send(Sock, Packet),
	{ok,_Data} = gen_tcp:recv(Sock, 0),
	ok = gen_tcp:close(Sock).

inet_parse_address(Host) ->
	[A,B,C,D] = string:tokens(Host, "."),
	{ok,{list_to_integer(A),
		 list_to_integer(B),
		 list_to_integer(C),
		 list_to_integer(D)}}.

%%EOF
