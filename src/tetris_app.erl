-module(tetris_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/", cowboy_static, {priv_file, tetris, "index.html"}},
                         {"/ws", bullet_handler, [{handler, tetris_bullet}]},
                         {"/js/bullet.js", cowboy_static, {priv_file, bullet, "bullet.js"}},
                         {"/static/[...]", cowboy_static, {priv_dir, tetris, "static"}}]}
                 ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    tetris_sup:start_link().

stop(_State) ->
    ok.
