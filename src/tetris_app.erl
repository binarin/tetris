-module(tetris_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export([update_dispatch/0]).
update_dispatch() ->
    cowboy:set_env(http, dispatch, cowboy_dispatch()).

cowboy_dispatch() ->
    cowboy_router:compile(
      [{'_', [{"/ws", bullet_handler, [{handler, tetris_bullet}]}
             ,{"/static/js/bullet.js", cowboy_static, {priv_file, bullet, "bullet.js"}}
             ,{"/static/[...]", cowboy_static, {priv_dir, tetris, "static"}}
             ,{"/[:page]", tetris_http_pages, []}
             ]}
      ]).

start(_Type, _Args) ->
    Dispatch = cowboy_dispatch(),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    tetris_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
    ok.
