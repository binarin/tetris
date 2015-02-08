-module(tetris_bullet).
-compile([{parse_transform, lager_transform}]).
-export([init/4, stream/3, info/3, terminate/2]).

-export([reset_board/4, current_block/4]).

init(_Transport, Req, _Opts, _Active = true) ->
    tetris_game:player_connected(tetris_game, 1, self()),
    {ok, Req, undefined_state};
init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, undefined_state}.

stream(Data, Req, State) ->
    Parsed = atomize_map_keys(jiffy:decode(Data, [return_maps])),
    lager:info("Stream received: ~p", [Parsed]),
    case Parsed of
        #{event := <<"keypress">>, key := Key} ->
            tetris_game:keypress(tetris_game, Key);
        _ ->
            lager:info("Command unknown: ~p", [Parsed])
    end,
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
info({current_block, Block, CurrentCol, CurrentRow}, Req, State) ->
    {reply, jiffy:encode({[{command, current_block},
                           {block, array:to_list(Block)},
                           {row, CurrentRow},
                           {col, CurrentCol}]}),
     Req, State};
info(Info, Req, State) ->
    lager:info("Info received: ~p", [Info]),
    {ok, Req, State}.

terminate(_Req, _State) ->
    tetris_game:player_disconnected(tetris_game, 1, self()),
    ok.


%% API
reset_board(PlayerSocket, Rows, Cols, Board) ->
    PlayerSocket ! {reset_board, Rows, Cols, Board}.

current_block(PlayerSocket, Block, CurrentCol, CurrentRow) ->
    PlayerSocket ! {current_block, Block, CurrentCol, CurrentRow}.

%% internal functions
atomize_map_keys(Map) ->
    maps:from_list([ {erlang:binary_to_atom(K, utf8), V} || {K, V} <- maps:to_list(Map)]).
