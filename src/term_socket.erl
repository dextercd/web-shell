-module(term_socket).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _State) ->
    {cowboy_websocket, Req, no_terminal}.

websocket_init(no_terminal) ->
    Terminal = pty_manager_server:create_terminal(),
    {ok, Terminal}.

websocket_handle({binary, Data}, Terminal) ->
    pty_manager_server:terminal_input(Terminal, Data),
    {ok, Terminal};
websocket_handle(_, Terminal) ->
    {ok, Terminal}.

websocket_info({terminal_output, Terminal, Output}, Terminal) ->
    {
        [{binary, Output}],
        Terminal
    }.

terminate(_Reason, _Req, Terminal) ->
    pty_manager_server:close_terminal(Terminal).
