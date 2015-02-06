-module(tetris_bullet).
-compile([{parse_transform, lager_transform}]).
-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, _Active = true) ->
    lager:info("Active init: ~p", [Req]),
    {ok, Req, undefined_state};
init(_Transport, Req, _Opts, _Active) ->
    {ok, Req, undefined_state}.

stream(Data, Req, State) ->
    lager:info("Stream received: ~p", [Req]),
    {reply, Data, Req, State}.

info(_Info, Req, State) ->
    lager:info("Info received: ~p", [Req]),
    {ok, Req, State}.

terminate(_Req, _State) ->
    ok.
