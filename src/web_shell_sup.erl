-module(web_shell_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 2,
                 period => 5},
    PtyManager = #{id => pty_manager_server,
                   start => {pty_manager_server, start_link, []}},
    ChildSpecs = [PtyManager],
    {ok, {SupFlags, ChildSpecs}}.
