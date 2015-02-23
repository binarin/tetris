%% @doc Helpers for handling initialization of state from
%% cowboy_req. This initialization is a series of transformations on a
%% cowboy request and a state, where state rerpesented as erlang map. 
-module(tetris_http_init).
-export([init_from_request/1, init_from_request/2]).

-type initializer() :: fun (({cowboy_req:req(), #{}}) -> {cowboy_req:req(), #{}}).

-define(DEFAULT_INITIALIZERS,
        [fun init_session/1,
         fun init_user/1]).

%% @doc Fetches session data for a http request (creating session if
%% it doesn't exists) and stores it under a 'session' key.
init_session({Req, State}) ->
    {ok, Session, Req2} = tetris_http_session:ensure_session(Req),
    {Req2, State#{session => Session}}.

%% @doc If session contains information about logged-in user, fetches
%% full information about this user and stores it under a 'user' key.
init_user({Req, #{session := Session} = State}) ->
    case tetris_http_session:get_value(user_id, Session) of
        undefined ->
            {Req, State#{user => undefined}};
        UserId when is_integer(UserId) ->
            {Req, State#{user => tetris_user:get_user(UserId)}}
    end.

%% @equiv init_from_request(Req, []).
-spec init_from_request(Req :: cowboy_req:req()) -> {Req2 :: cowboy_req:req(), State :: #{}}.
init_from_request(Req) ->
    init_from_request(Req, []).

%% @doc Given a list of initializers, executes them step by
%% step. Default initializers from this modules are always added to
%% beginning of the initializers list.
-spec init_from_request(Req :: cowboy_req:req(), CustomInitializers :: [initializer()]) -> {Req2 :: cowboy_req:req(), State :: #{}}.
init_from_request(Req, CustomInitializers) ->
    lists:foldl(fun (InitFun, Acc) -> InitFun(Acc) end,
                {Req, #{}},
                ?DEFAULT_INITIALIZERS ++ CustomInitializers).
