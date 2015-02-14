%%%-------------------------------------------------------------------
%%% @doc
%%% Standalone tetris game (board for one player).
%%% @end
%%%-------------------------------------------------------------------
-module(tetris_game).
-compile([{parse_transform, lager_transform}, export_all]).

-behaviour(gen_server).


%% API
-export([start/0, start_link/0, player_connected/3, player_disconnected/3,
         keypress/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).
-define(ROWS, 20).
-define(COLS, 10).

-type board() :: array:array(integer()).
-type block() :: array:array(integer()).

-define(NUM_BLOCKS, 5).

%% Blocks are rotated around center, clockwise.
-define(BLOCKS,
        [ "1111"
          "0000"
          "0000"
          "0000",

          "0110"
          "1100"
          "0000"
          "0000",

          "1100"
          "0110"
          "0000"
          "0000",

          "1100"
          "1100"
          "0000"
          "0000",

          "0100"
          "1110"
          "0000"
          "0000"
        ]).

-record(state, {player_id,
                player_socket,
                board,
                speed,  %% milliseconds between automatic block movements
                block_move_timer,
                current_block,
                current_col,
                current_row}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

player_connected(Server, PlayerId, PlayerSocket) ->
    gen_server:call(Server, {player_connected, PlayerId, PlayerSocket}).

player_disconnected(Server, _PlayerId, _PlayerSocket) ->
    gen_server:call(Server, player_disconnected).

keypress(Server, Key) ->
    gen_server:call(Server, {keypress, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(player_disconnected, _From, #state{player_id = PlayerId} = State) ->
    lager:info("Player ~p disconnected", [PlayerId]),
    State1 = stop_game(State),
    State2 = State1#state{player_id = undefined},
    {reply, ok, State2};
handle_call({player_connected, PlayerId, PlayerSocket}, _From, #state{player_id = PlayerId} = State) ->
    lager:info("Player ~p reconnected (socket ~p)", [PlayerId, PlayerSocket]),
    {reply, ok, State#state{player_socket = PlayerSocket}};
handle_call({player_connected, PlayerId, PlayerSocket}, _From, #state{player_id = undefined} = State) ->
    lager:info("Player ~p connected, starting game (socket ~p)", [PlayerId, PlayerSocket]),
    State1 = start_game(State#state{player_id = PlayerId,
                                    player_socket = PlayerSocket}),
    tetris_bullet:reset_board(PlayerSocket, ?ROWS, ?COLS, State1#state.board),
    tetris_bullet:current_block(PlayerSocket, State1#state.current_block, State1#state.current_row, State1#state.current_col),
    {reply, ok, State1};
handle_call({keypress, _Key} = Event, From, State) ->
    {reply, ok, handle_keypress_call(Event, From, State)};
handle_call({player_connect, NewPlayerId, _PlayerSocket}, _From, #state{player_id = OldPlayerId} = State) ->
    lager:info("Game already have another player ~p, ~p is not allowed to connect", [OldPlayerId, NewPlayerId]),
    {reply, ok, State};
handle_call({add_block, _X, _Y} = Msg, _From, #state{player_socket=PlayerSocket} = State) ->
    PlayerSocket ! Msg,
    {reply, ok, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(move_block, #state{block_move_timer = undefined} = State) ->
    %% Game was stopped, it's leftover message, just ignore it.
    {noreply, State};
handle_info(move_block, State) ->
    {noreply, move_block(State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Moves block one row down. If it's not possible - adds block to
%% game board, and initializes new block movement. Returns new state.
-spec move_block(State:: #state{}) -> #state{}.
move_block(#state{current_row = Row, current_col = Col, current_block = Block, board = Board} = State) ->
    State1 = case valid_position(Board, Row + 1, Col, Block) of
                 true ->
                     State#state{current_row = Row + 1};
                 false ->
                     NewBoard = fix_block_at_board(Board, Row, Col, Block),
                     tetris_bullet:reset_board(State#state.player_socket, ?ROWS, ?COLS, NewBoard),
                     State#state{board = NewBoard,
                                 current_block = random_block(),
                                 current_row = 0,
                                 current_col = 3}
             end,
    tetris_bullet:current_block(State1#state.player_socket, State1#state.current_block, State1#state.current_row, State1#state.current_col),
    State1.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_empty_board() ->
    array:new([?ROWS * ?COLS, {default, 0}]).

random_block() ->
    get_block_as_array(random:uniform(?NUM_BLOCKS) - 1).

start_game(State) ->
    {ok, TRef} = timer:send_interval(500, move_block),
    State#state{
      speed = 500,
      board = make_empty_board(),
      current_block = random_block(),
      current_row = 0,
      current_col = 3,
      block_move_timer = TRef}.

stop_game(State) ->
    {ok, cancel} = timer:cancel(State#state.block_move_timer),
    State#state{block_move_timer = undefined}.

handle_keypress_call({keypress, <<"up">>}, _From, State) ->
    NewBlock = rotate_block(State#state.current_block, 1),
    case valid_position(State#state.board, State#state.current_row, State#state.current_col, NewBlock) of
        true ->
            tetris_bullet:current_block(State#state.player_socket, NewBlock,
                                        State#state.current_row, State#state.current_col),
            State#state{current_block = NewBlock};
        false ->
            State
    end;
handle_keypress_call({keypress, _Key}, _From, State) ->
    State.

%%%===================================================================
%%% Game board handling
%%%===================================================================
%% add_bottom_line(#state{board = Board} = State, FillPercent) ->
%%     {_EmptyDiscard, Rest} = lists:split(?COLS, array:to_list(Board)),
%%     NewLine = [case random:uniform(100) of
%%                    X when X < FillPercent -> true;
%%                    _ -> false
%%                end || _ <- lists:seq(1, ?COLS)],
%%     State#state{board = array:from_list(Rest ++ NewLine, false)}.


get_block_as_list(BlockNumber) ->
    [ X - $0 || X <- lists:nth(BlockNumber + 1, ?BLOCKS) ].

get_block_as_array(BlockNumber) ->
    array:from_list(get_block_as_list(BlockNumber), 0).

%%--------------------------------------------------------------------
%% @doc
%%
%% Converts tetramino number and its rotation to 16-element array,
%% representing the block in its fullest.  
%%
%% @spec
%% @end
%%--------------------------------------------------------------------
expand_block(BlockNumber, Rotation) ->
    InitialBlock = get_block_as_array(BlockNumber),
    rotate_block(InitialBlock, Rotation).

rotate_block(BlockArray, 0) ->
    strip_margins(BlockArray, margin_top(BlockArray), margin_left(BlockArray));
rotate_block(BlockArray, N) ->
    Rotated = array:map(
                fun(Idx, _Value) ->
                        Row = Idx div 4,
                        Col = Idx rem 4,
                        array:get((3 - Col) * 4 + Row, BlockArray)
                end,
                array:new([16, fixed, {default, 0}])),
    rotate_block(Rotated, N - 1).


%% @doc Block has reached it's final destination, add it as a static board content.
-spec fix_block_at_board(Board :: board(), Row :: integer(), Col :: integer(), Block :: block()) -> board().
fix_block_at_board(Board, Row, Col, Block) ->
    array:map(
      fun(Idx, Value) ->
              BoardRow = Idx div ?COLS,
              BoardCol = Idx rem ?COLS,
              %% This cell coordinates in block coordinate space
              BlockRow = BoardRow - Row,
              BlockCol = BoardCol - Col,
              if
                  BlockRow >= 0 andalso BlockRow < 4 andalso BlockCol >= 0 andalso BlockCol < 4 ->
                      case array:get(BlockRow * 4 + BlockCol, Block) of
                          0 -> Value;
                          BlockValue -> BlockValue
                      end;
                  true ->
                      Value
              end
      end,
      Board).

%%--------------------------------------------------------------------
%% @doc Checks whether we could place given block at a given position.
%% @end
%%--------------------------------------------------------------------
-spec valid_position(Board :: board(), Row :: integer(), Column :: integer(), Block :: block()) -> boolean().
valid_position(Board, Row, Column, Block) ->
    array:foldl(
      fun (Idx, Value, Acc) ->
              PosY = Row + Idx div 4,
              PosX = Column + Idx rem 4,
              BlockElementNeedsToBePlaced = Value > 0,
              HasEmptySpaceAtPos =
                  if
                      PosY < 0 orelse PosY >= ?ROWS -> false;
                      PosX < 0 orelse PosX >= ?COLS -> false;
                      true -> array:get(PosY * ?COLS + PosX, Board) =:= 0
                  end,
              if
                  Acc =:= false ->
                      false;
                  BlockElementNeedsToBePlaced and not HasEmptySpaceAtPos ->
                      false;
                  true -> true
              end
      end,
      true,
      Block).


%% @doc Calculates how many empty rows has array representing
%% block. This margin is a result of block rotation which we are going
%% to clean up later.
%% @end
margin_top(BlockArray) ->
    %% Location of first non-zero element
    {ZeroList, _} = lists:splitwith(fun (A) -> A == 0 end, array:to_list(BlockArray)),
    length(ZeroList) div 4.

%% @doc Calculates how many empty columns has array representing
%% block. This margin is a result of block rotation which we are going
%% to clean up later.
%% @end
margin_left(BlockArray) ->
    margin_left(array:to_list(BlockArray), 4).

margin_left([], SoFar) ->
    SoFar;
margin_left([0, 0, 0, 0 | Rest], SoFar) ->
    margin_left(Rest, SoFar);
margin_left([0, 0, 0, X | Rest], SoFar) when X > 0 ->
    margin_left(Rest, min(3, SoFar));
margin_left([0, 0, X, _ | Rest], SoFar) when X > 0 ->
    margin_left(Rest, min(2, SoFar));
margin_left([0, X, _, _ | Rest], SoFar) when X > 0 ->
    margin_left(Rest, min(1, SoFar));
margin_left(_, _) ->
    0.

%% @doc Removes given number of rows and column from top and left side of the block, 
%% to aleviate rotation side-effects.
%% @end
strip_margins(BlockArray, StripRows, StripCols) ->
    io:format("Strip ~p - ~p,~p~n", [BlockArray, StripRows, StripCols]),
    array:from_list(
      lists:map(
        fun (Idx) ->
                TgtRow = Idx div 4,
                TgtCol = Idx rem 4,
                SrcRow = TgtRow + StripRows,
                SrcCol = TgtCol + StripCols,
                if
                    SrcRow >= 4 orelse SrcCol >= 4 -> 0;
                    true -> array:get(SrcRow * 4 + SrcCol, BlockArray)
                end
        end,
        lists:seq(0, 15)),
      0).



%%%===================================================================
%%% Tests
%%%===================================================================
expand_block_test() ->
    ToString = fun(Block) ->
                       [ $0 + X || X <- array:to_list(Block) ]
               end,
    ?assertEqual(
       "0100"
       "1110"
       "0000"
       "0000",
       ToString(expand_block(4, 0))),
    ?assertEqual(
       "1000"
       "1100"
       "1000"
       "0000",
       ToString(expand_block(4, 1))),
    ?assertEqual(
       "1110"
       "0100"
       "0000"
       "0000",
       ToString(expand_block(4, 2))),
    ?assertEqual(
       "0100"
       "1100"
       "0100"
       "0000",
       ToString(expand_block(4, 3))).

valid_position_test() ->
    BlockStr =
        "0000"
        "0110"
        "0011"
        "0000",
    BoardStr = 
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "0000000000"
        "1000000000"
        "0100000000"
        "0010000000"
        "0001000000",
    Block = array:from_list([ X - $0 || X <- BlockStr ], 0),
    Board = array:from_list([ X - $0 || X <- BoardStr ], 0),
    ?assert(valid_position(Board, 0, 0, Block)),
    ?assert(valid_position(Board, 0, -1, Block)),
    ?assert(valid_position(Board, -1, 0, Block)),
    ?assertNot(valid_position(Board, -2, 0, Block)),
    ?assertNot(valid_position(Board, 0, -2, Block)),
    ?assert(valid_position(Board, 0, 6, Block)),
    ?assertNot(valid_position(Board, 0, 7, Block)),
    ?assert(valid_position(Board, 10, 3, Block)),
    ?assert(valid_position(Board, 17, -1, Block)),
    ?assertNot(valid_position(Board, 17, 0, Block)),
    ?assertNot(valid_position(Board, 18, 5, Block)),
    ?assertNot(valid_position(Board, 17, 7, Block)),
    ok.

margin_left_test() ->
    BlockStr =
        "0000"
        "0010"
        "0011"
        "0010",
    Block = array:from_list([ X - $0 || X <- BlockStr ], 0),
    ?assertEqual(2, margin_left(Block)).

margin_top_test() ->
    FromStr = fun (Str) ->
                      array:from_list([ X - $0 || X <- Str ], 0)
              end,
    ?assertEqual(0, margin_top(FromStr("0100"
                                       "0000"
                                       "0000"
                                       "0010"))),
    ?assertEqual(1, margin_top(FromStr("0000"
                                       "0100"
                                       "0000"
                                       "0010"))),
    ?assertEqual(2, margin_top(FromStr("0000"
                                       "0000"
                                       "0010"
                                       "0010"))),
    ?assertEqual(3, margin_top(FromStr("0000"
                                       "0000"
                                       "0000"
                                       "0010"))).

strip_margins_test() ->
    FromString = fun(Str) ->
                         array:from_list([ X - $0 || X <- Str ], 0)
                 end,
    ToString = fun(Block) ->
                       [ $0 + X || X <- array:to_list(Block) ]
               end,
    ?assertEqual("1000"
                 "1100"
                 "1000"
                 "0000",
                 ToString(strip_margins(FromString("0000"
                                                   "0010"
                                                   "0011"
                                                   "0010"), 1, 2))).
