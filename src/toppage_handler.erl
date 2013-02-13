-module(toppage_handler).
-export([init/3]).
-export([handle/2,terminate/3]).

init({tcp,http}, Req, []) ->

	TsProxyReceived = zergling_app:timestamp(),

	keeper ! {get,self()},
	OtherVars = receive {vars,X} -> X end,

	TsReqReceived = proplists:get_value(ts_req_received, OtherVars),
	TsLingStarted = proplists:get_value(ts_ling_started, OtherVars),
	TsBootStarted = proplists:get_value(ts_boot_started, OtherVars),
	TsAppStarted = proplists:get_value(ts_app_started, OtherVars),
	TsCowboyStarted = proplists:get_value(ts_cowboy_started, OtherVars),
	TsSpawnerNotified = proplists:get_value(ts_spawner_notified, OtherVars),

	IntLingStarted = (TsLingStarted - TsReqReceived) *1000,
	IntBootStarted = (TsBootStarted - TsLingStarted) *1000,
	IntAppStarted = (TsAppStarted - TsBootStarted) *1000,
	IntCowboyStarted = (TsCowboyStarted - TsAppStarted) *1000,
	IntSpawnerNotified = (TsSpawnerNotified - TsCowboyStarted) *1000,
	IntProxyReceived = (TsProxyReceived - TsSpawnerNotified) *1000,

	TotalSec = TsProxyReceived - TsReqReceived,

	CmdLine = io_lib:format("~p", [init:get_arguments()]),
	Vars = [{cmd_line,CmdLine},
			{ts_proxy_received,TsProxyReceived},
			{int_ling_started,IntLingStarted},
			{int_boot_started,IntBootStarted},
			{int_app_started,IntAppStarted},
			{int_cowboy_started,IntCowboyStarted},
			{int_spawner_notified,IntSpawnerNotified},
			{int_proxy_received,IntProxyReceived},
			{total_sec,TotalSec}]

		++ OtherVars,

	{ok,Body} = welcome_dtl:render(Vars),

	{ok,Reply} = cowboy_req:reply(200, [{<<"connection">>,<<"close">>}], Body, Req),

	{shutdown,Reply,undefined}.

handle(Req, St) ->
	{ok,Req,St}.	%% never reached

terminate(_What, _Req, _St) ->
	
	%% A single request per instance
	init:stop().

%%EOF

