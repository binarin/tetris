-module(tetris_http_pages).
-behaviour(cowboy_http_handler).
-compile([{parse_transform, lager_transform}]).

-export([init/3, handle/2, terminate/3]).

init({_TransportName, _ProtocolName}, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {Session, _} = cowboy_req:meta(session, Req),
    lager:info("~p", [Session]),
    {ok, Body} = login_dtl:render([]),
    {ok, Req2} = cowboy_req:reply(
                   200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}],
                   Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

