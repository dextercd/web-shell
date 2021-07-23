-module(simple_template).

-behaviour(cowboy_rest).

-export([init/2, to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

to_html(Req, State) ->
    {ok, Result} = State:render(),
    {Result, Req, State}.
