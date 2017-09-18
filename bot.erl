-module(bot).
-author('Mytchel Hammond <mytch@lackname.org>').

-export([run/4]).

run(Address, Port, Nick, Channel) ->
	{ok, Sock} = gen_tcp:connect(Address, Port, 
	                            [binary, {packet, 0}, {active, false}]),
	ok = gen_tcp:send(Sock, 
	       lists:concat(["USER ", Nick, " ", Nick, " ", Nick, " :A Bot\r\n"])),
	ok = gen_tcp:send(Sock, 
	       lists:concat(["NICK ", Nick, "\r\n"])),
	ok = gen_tcp:send(Sock, 
	       lists:concat(["JOIN ", Channel, "\r\n"])),
	ets:new(tells, [bag, named_table]),
	loop(Sock).

loop(Sock) ->
	case gen_tcp:recv(Sock, 0) of
		{ok, Data} ->
			io:format("recv ~p ~n", [Data]),
			ok = respond(Sock, string:tokens(binary_to_list(Data), " \r\n")),
			loop(Sock);
		{error, Reason} ->
			{error, Reason}
	end. 

getNick(Full) ->
	lists:nth(2, re:split(Full, ":|!", [{return, list}])).

respond(Sock, ["PING", Message]) ->
	gen_tcp:send(Sock, lists:concat(["PONG ", Message]));

respond(Sock,  [From, "PRIVMSG", Chan, ":" ++ MessageHead | MessageTail]) ->
	User = getNick(From),
	case MessageHead of
		"." ++ Cmd -> 
			command(Sock, Chan, User, Cmd, MessageTail);
		_ -> 
			Message = string:join([MessageHead | MessageTail], " "),
			answer(Sock, Chan, User, Message)
	end;
	
respond(Sock,  [From, "JOIN", ":" ++ Chan]) ->
	User = getNick(From),
	sendTells(Sock, Chan, User, ets:lookup(tells, User));

respond(_, _) ->
	ok.


answer(Sock, Chan, User, _) ->
	ok = sendTells(Sock, Chan, User, ets:lookup(tells, User)),
	ok.



sendTells(_, _, _, []) ->
	ok;
	
sendTells(Sock, Chan, User, [{To, From, Message} |T]) ->
	Resp = io_lib:format("~s: ~s told me to say: ~s", 
	              [To, From, Message]),
	ets:delete_object(tells, {To, From, Message}),
	gen_tcp:send(Sock, 
		lists:concat(["PRIVMSG ", Chan, " :", Resp, "\r\n"])),
	sendTells(Sock, Chan, User, T).
	


command(Sock, Chan, _, "help", ["tell" | _]) ->
	gen_tcp:send(Sock, 
		lists:concat(["PRIVMSG ", Chan, " :usage: .tell [nick] a message\r\n"]));

command(Sock, Chan, _, "help", []) ->
	gen_tcp:send(Sock, 
		lists:concat(["PRIVMSG ", Chan, " :I know how to: tell, and not much else.\r\n"]));



command(Sock, Chan, User, "tell", [To | SplitMessage]) ->
	Message = string:join(SplitMessage, " "),
	ets:insert(tells, {To, User, Message}),
	Resp = io_lib:format("~s: I will tell ~s that next time I see them.",
	                     [User, To]),
	gen_tcp:send(Sock, 
		lists:concat(["PRIVMSG ", Chan, " :", Resp, "\r\n"]));

command(Sock, Chan, User, Cmd, _) ->
	Resp = io_lib:format("I do not know what ~p means ~p", 
	              [Cmd, User]),
	gen_tcp:send(Sock, 
		lists:concat(["PRIVMSG ", Chan, " :", Resp, "\r\n"])).
