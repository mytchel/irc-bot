-module(bot).
-author('Mytchel Hammond <mytch@lackname.org>').

-export([start/4]).

start(Address, Port, Nick, Channel) ->
	{ok, Sock} = gen_tcp:connect(Address, Port, 
	                            [binary, {packet, 0}, {active, false}]),
	ok = gen_tcp:send(Sock, 
	       lists:concat(["USER ", Nick, " ", Nick, " ", Nick, " :A Bot\r\n"])),
	ok = gen_tcp:send(Sock, 
	       lists:concat(["NICK ", Nick, "\r\n"])),
	ok = gen_tcp:send(Sock, 
	       lists:concat(["JOIN ", Channel, "\r\n"])),
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

respond(Sock, ["PING", Message]) ->
	gen_tcp:send(Sock, lists:concat(["PONG ", Message]));

respond(Sock,  [From, "PRIVMSG", Channel, ":" ++ MessageHead | MessageTail]) ->
	User = lists:nth(2, re:split(From, ":|!", [{return, list}])),
	Resp = case MessageHead of
		"." ++ Cmd -> 
			command(User, Cmd, MessageTail);
		_ -> 
			Message = string:join([MessageHead | MessageTail], " "),
			answer(User, Message)
	end,
	case Resp of 
		nothing -> ok;
		_ -> gen_tcp:send(Sock, lists:concat(["PRIVMSG ", Channel, " :", Resp, "\r\n"]))
	end;

respond(_, _) ->
	ok.

answer(_, _) ->
	nothing.

command(_, "help", ["tell" | _]) ->
	io_lib:format("usage: .tell [nick] a message.", []);

command(_, "help", []) ->
	io_lib:format("I know how to: tell, and not much else", []);

command(User, "tell", [To | SplitMessage]) ->
	Message = string:join(SplitMessage, " "),
	io_lib:format("Should tell ~p ~p from ~p",
	              [To, Message, User]);

command(User, Cmd, _) ->
	io_lib:format("I do not know what ~p means ~p", 
	              [Cmd, User]).
