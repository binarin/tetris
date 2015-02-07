%%%-------------------------------------------------------------------
%%% @author Alexey Lebedeff <binarin@binarin.ru>
%%% @copyright (C) 2015, Alexey Lebedeff
%%% @doc
%%% Standalone tetris game (board for one player).
%%% @end
%%% Created :  6 Feb 2015 by Alexey Lebedeff <binarin@binarin.ru>
%%%-------------------------------------------------------------------
-module(tetris_game).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, player_connected/3, player_disconnected/3,
         keypress/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ROWS, 20).
-define(COLS, 10).

-define(NUM_BLOCKS, 5).
%% block rotation is number between 1 and 4, 1 being "no rotation" (as
%% in ?BLOCKS below), 2 is 90deg clockwise and so on.
-define(BLOCKS, [[ 1, 1, 1, 1],
                 
                 [ 1, 1, 0, 0,
                   0, 1, 1, 0 ],

                 [ 0, 1, 1, 0,
                   1, 1 ],

                 [ 1, 1, 0, 0,
                   1, 1 ],

                 [ 0, 1, 0, 0,
                   1, 1, 1 ]]).

-record(state, {player_id,
                player_socket,
                board,
                speed,  %% milliseconds between automatic block movements
                last_move_time,
                current_block,
                current_rotation}).

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
handle_call(player_disconnected, _From, #state{player_id = PlayerId}) ->
    lager:info("Player ~p disconnected", [PlayerId]),
    {reply, ok, #state{}};
handle_call({player_connected, PlayerId, PlayerSocket}, _From, #state{player_id = PlayerId} = State) ->
    lager:info("Player ~p reconnected (socket ~p)", [PlayerId, PlayerSocket]),
    {reply, ok, State#state{player_socket = PlayerSocket}};
handle_call({player_connected, PlayerId, PlayerSocket}, _From, #state{player_id = undefined} = State) ->
    lager:info("Player ~p connected, starting game (socket ~p)", [PlayerId, PlayerSocket]),
    State1 = start_game(State#state{player_id = PlayerId,
                                    player_socket = PlayerSocket}),
    tetris_bullet:reset_board(PlayerSocket, ?ROWS, ?COLS, State1#state.board),
    {reply, ok, State1, move_timeout(State1)};
handle_call({keypress, _Key} = Event, From, State) ->
    handle_keypress_call(Event, From, State);
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
handle_info(timeout, State) ->
    lager:info("Move timeout happened"),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

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
    array:new([?ROWS * ?COLS, {default, false}]).

microtime() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MicroSecs + 1000000 * (Secs + 1000000 * MegaSecs).

random_block() ->
    {lists:nth(random:uniform(?NUM_BLOCKS), ?BLOCKS),
     random:uniform(4)}.

start_game(State) ->
    {Block, Rotation} = random_block(),
    State#state{
      speed = 500,
      board = make_empty_board(),
      current_block = Block,
      current_rotation = Rotation,
      last_move_time = microtime()}.

move_timeout(#state{speed = Speed, last_move_time = LastMoveTime}) ->
    case LastMoveTime + Speed - microtime() of
        NoMoreLeft when NoMoreLeft =< 0 ->
            0;
        MicroSecondsLeft ->
            MicroSecondsLeft div 1000
    end.

handle_keypress_call({keypress, Key}, _From, #state{player_socket = PlayerSocket} = State) ->
    lager:info("Keypress from client ~p", [Key]),
    State1 = add_bottom_line(State, 70),
    tetris_bullet:reset_board(PlayerSocket, ?ROWS, ?COLS, State1#state.board),
    {reply, ok, State1, move_timeout(State1)}.

%%%===================================================================
%%% Game board handling
%%%===================================================================
add_bottom_line(#state{board = Board} = State, FillPercent) ->
    lager:info("Board ~p", [Board]),
    {_EmptyDiscard, Rest} = lists:split(?COLS, array:to_list(Board)),
    NewLine = [case random:uniform(100) of
                   X when X < FillPercent -> true;
                   _ -> false
               end || _ <- lists:seq(1, ?COLS)],
    State#state{board = array:from_list(Rest ++ NewLine, false)}.
