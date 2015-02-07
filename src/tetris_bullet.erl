-module(tetris_bullet).
-compile([{parse_transform, lager_transform}]).
-export([init/4, stream/3, info/3, terminate/2]).

-export([reset_board/4]).

init(_Transport, Req, _Opts, _Active = true) ->
    tetris_game:player_connected(tetris_game, 1, self()),
    {ok, Req, undefined_state};
init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, undefined_state}.

stream(Data, Req, State) ->
    lager:info("Stream received: ~p", [Req]),
    {reply, Data, Req, State}.

info({reset_board, Rows, Cols, Board}, Req, State) ->
    {reply, jiffy:encode({[{command, reset_board},
                           {width, Cols},
                           {height, Rows},
                           {board, array:to_list(Board)}]}),
     Req, State};
info({add_block, X, Y}, Req, State) ->
    {reply, jiffy:encode({[{command, add_block},
                           {data, [X, Y]}]}),
     Req, State
    };
info(_Info, Req, State) ->
    lager:info("Info received: ~p", [Req]),
    {ok, Req, State}.

terminate(_Req, _State) ->
    tetris_game:player_disconnected(tetris_game, 1, self()),
    ok.


%% API
reset_board(PlayerSocket, Rows, Cols, Board) ->
    PlayerSocket ! {reset_board, Rows, Cols, Board}.
