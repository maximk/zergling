-module(toppage_handler).
-export([init/3]).
-export([handle/2,terminate/3]).

init({tcp,http}, Req, []) ->

	CmdLine = io_lib:format("~p", [init:get_arguments()]),
	Vars = [{node,node()},
			{cmd_line,CmdLine}],
	{ok,Body} = welcome_dtl:render(Vars),

	{ok,Reply} = cowboy_req:reply(200, [{<<"connection">>,<<"close">>}], Body, Req),

	{shutdown,Reply,undefined}.

handle(Req, St) ->
	{ok,Req,St}.	%% never reached

terminate(_What, _Req, _St) ->
	
	%% A single request per instance
	init:stop().

%%EOF

