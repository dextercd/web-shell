-module(web_shell_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    compile_templates(),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", simple_template, index_template},

            % Default nor extended mimetypes have the wasm mime, for simplicity
            % we just hard code it for this one thing.
            {"/static/terminal.wasm", cowboy_static,
                {priv_file, web_shell, "static/terminal.wasm",
                 [{mimetypes, {<<"application">>, <<"wasm">>, []}}]}},

            {"/static/[...]", cowboy_static, {priv_dir, web_shell, "static"}},

            {"/terminal", term_socket, {}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    web_shell_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(my_http_listener),
    ok.

priv_file(Paths) ->
    PrivDir = code:priv_dir(web_shell),
    filename:join([PrivDir|Paths]).

template_path(Paths) ->
    priv_file(["templates"|Paths]).

index_data() ->
    {ok, ManifestJson} = file:read_file(priv_file(["static/dist/manifest.json"])),
    Manifest = jsx:decode(ManifestJson),
    MainName = maps:get(<<"main.js">>, Manifest),
    RuntimeName = maps:get(<<"runtime.js">>, Manifest),
    {ok, RuntimeContents} = file:read_file(priv_file(["static/dist/", RuntimeName])),
    {MainName, RuntimeContents}.

compile_templates() ->
    {MainName, RuntimeContents} = index_data(),
    erlydtl:compile_file(template_path(["index.dtl"]), index_template,
        [{out_dir, false},
         {constants, [{mainjs, MainName}, {runtime_contents, RuntimeContents}]}]),
    ok.
