-module(term_socket).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _State) ->
    % Set timeout to 2 minutes.
    % This is necessary because Chrome puts pages in 'low power' mode after 5
    % minutes of inactivity. In low power mode (JavaScript) timers are checked
    % every minute, instead of regularly. This means that the default of 60
    % seconds won't work well on Chrome.
    {cowboy_websocket, Req, no_terminal,
        #{idle_timeout => 2 * 60 * 1000}}.

websocket_init(no_terminal) ->
    Terminal = pty_manager_server:create_terminal(),
    {ok, Terminal}.

websocket_handle({binary, <<$w, Data/binary>>}, Terminal) ->
    pty_manager_server:terminal_input(Terminal, Data),
    {ok, Terminal};
websocket_handle({binary, <<$r, Width:32, Height:32>>}, Terminal) ->
    pty_manager_server:terminal_resize(Terminal, Width, Height),
    {ok, Terminal};
websocket_handle(_, Terminal) ->
    {ok, Terminal}.

websocket_info({terminal_resize, Terminal, {Width, Height}}, Terminal) ->
    {
        [{binary, [$r, <<Width:32, Height:32>>]}],
        Terminal
    };
websocket_info({terminal_output, Terminal, Output}, Terminal) ->
    {
        [{binary, [$o, Output]}],
        Terminal
    }.

terminate(_Reason, _Req, Terminal) ->
    pty_manager_server:close_terminal(Terminal).
