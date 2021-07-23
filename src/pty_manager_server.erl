-module(pty_manager_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/0, create_terminal/0, terminal_input/2, close_terminal/1]).

-record(state,
    {
        port,
        terminal_owners=#{}
    }).

start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    {ok, Pid}.

create_terminal() ->
    gen_server:call(?MODULE, {create_terminal, self()}).

terminal_input(Terminal, Data) ->
    gen_server:cast(?MODULE, {terminal_input, Terminal, Data}).

close_terminal(Terminal) ->
    gen_server:cast(?MODULE, {close_terminal, Terminal}).

start_manager_program() ->
    PrivDir = code:priv_dir(web_shell),
    PortPath = filename:join([PrivDir, "pty_mngr/pty_mngr"]),
    Port = erlang:open_port({spawn_executable, PortPath},
                            [{packet, 4}, binary, exit_status]),
    Port.

init(_) ->
    Port = start_manager_program(),
    {ok, #state{port=Port}}.

response_token() ->
    <<(rand:uniform(16#ffffffff)):32>>.

check_session({TerminalId, SessionKey}, #state{terminal_owners=Owners}) ->
    case maps:get(TerminalId, Owners, not_found) of
        {_, SessionKey} -> true;
        _ -> false
    end.

handle_call({create_terminal, Owner},
            _From,
            #state{port=Port, terminal_owners=Owners}=State) ->
    RespToken = response_token(),
    Port ! {self(), {command, <<"c", RespToken:4/binary>>}},
    receive
        {Port, {data, <<"r", RespToken:4/binary, TerminalId:8/binary>>}} ->
            SessionKey = crypto:strong_rand_bytes(10),
            Terminal = {TerminalId, SessionKey},
            NewState = State#state{
                terminal_owners=Owners#{TerminalId => {Owner, SessionKey}}},
            {reply, Terminal, NewState}
    end.

handle_cast({terminal_input, {TerminalId, _}=Terminal, Data},
            #state{port=Port} = State) ->
    case check_session(Terminal, State) of
        true ->
            Port ! {self(), {command, <<"w", TerminalId/binary, Data/binary>>}};
        false ->
            false
    end,
    {noreply, State};
handle_cast({close_terminal, {TerminalId, _}=Terminal},
            #state{port=Port} = State) ->
    case check_session(Terminal, State) of
        true ->
            Port ! {self(), {command, <<"k", TerminalId/binary>>}};
        false ->
            false
    end,
    {noreply, State}.

% Terminal Output
handle_info({Port, {data, <<MessageType, TerminalId:8/binary, Data/binary>>}},
            #state{port=Port, terminal_owners=Owners}=State) ->
    #{TerminalId := {Owner, SessionKey}} = Owners,
    case {MessageType, Data} of
        % Terminal Output
        {$o, <<Output/binary>>} ->
            Owner ! {terminal_output, {TerminalId, SessionKey}, Output},
            {noreply, State};

        % Terminal Exit
        {$x, <<Status:32>>} ->
            Message = list_to_binary(
                io_lib:format("\r\nProcess stopped with status: ~p\r\n", [Status])),
            Owner ! {terminal_output, {TerminalId, SessionKey}, Message},
            NewState = State#state{terminal_owners=maps:remove(TerminalId, Owners)},
            {noreply, NewState};

        _ ->
            io:format("Unknown Terminal Host message '~c', size: ~p~n",
                      [MessageType, byte_size(Data)]),
            {noreply, State}
    end;
handle_info(Other, State) ->
    io:format("Unhandled message: ~p~n", [Other]),
    {noreply, State}.
