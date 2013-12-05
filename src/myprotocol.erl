-module(myprotocol).
-compile([{parse_transform, lager_transform}]).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    lager:info("~p User has logged in", [self()]),
    Transport:send(Socket, <<"Welcome to Expression Calculator\nType 'quit' to end this session\n\n">>),
    loop(Socket, Transport, dict:new()).

loop(Socket, Transport, Context) ->
    {OK, Closed, Error} = Transport:messages(),
    Transport:setopts(Socket, [{active, once}]),
        receive
            {OK, Socket, Data} ->
                case handle_data(Data, Context) of
                    {reply, Answer, NewContext} -> Transport:send(Socket, Answer), loop(Socket, Transport, NewContext);
                    noreply         -> loop(Socket, Transport, Context);
                    quit            -> Transport:send(Socket, <<"Bye bye\n">>), close(Transport, Socket);
                    _               -> Transport:send(Socket, <<"Internal error\n">>), close(Transport, Socket)
                end;
            {Closed, Socket} ->
                lager:info("~p User has disconnected. Socket closed", [self()]),
                ok;
            {Error, Socket, _} ->
                ok = close(Transport, Socket)
        end.

handle_data(<<"\r", _/binary>>, _Context) ->
    noreply;
handle_data(<<"quit", _/binary>>, _Context) -> 
    quit;
handle_data(Data, Context) ->
    try calculator:solve(binary_to_list(Data), Context) of
        {Result, NewContext} ->
            { reply, io_lib:format("~p\n", [Result]), NewContext }
    catch
        {error, _ErrorContext, Message, Values} ->
            lager:error("~p ~p ~p", [self(), Message, Values]),
            {reply, io_lib:format("error: ~p ~p~n", [Message, Values]), Context};
        {error, _ErrorContext, Message} ->
            lager:error("~p ~p", [self(), Message]),
            {reply, io_lib:format("error: ~p~n", [Message]), Context}
    end.

close(Transport, Socket) ->
    lager:info("~p User has disconnected", [self()]),
    ok = Transport:close(Socket).