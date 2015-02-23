-module(tetris_http_pages).
-behaviour(cowboy_http_handler).
-compile([{parse_transform, lager_transform}]).

-export([init/3, handle/2, terminate/3]).

-include("tetris_user.hrl").

init_bindings({Req, State}) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    State2 = State#{page => proplists:get_value(page, Bindings)},
    {Req2, State2}.

init_method({Req, State}) ->
    {Method, Req2} = cowboy_req:method(Req),
    State2 = State#{method => Method},
    {Req2, State2}.

init({_TransportName, _ProtocolName}, Req, _Opts) ->
    {Req1, State1} = tetris_http_init:init_from_request(
                       Req, [fun init_bindings/1,
                             fun init_method/1]),
    {ok, Req1, State1}.

respond_template(Template, Vars, Req, State) ->
    {ok, Body} = Template:render(Vars),
    {ok, Req2} = cowboy_req:reply(
                   200, [{<<"content-type">>, <<"text/html; charset=utf-8">>}],
                   Body, Req),
    {ok, Req2, State}.

redirect(Location, Req, State) ->
    {ok, Req2} = cowboy_req:reply(302, [{<<"location">>, Location}], Req),
    {ok, Req2, State}.

try_login(Req, State) ->
    {ok, Form, Req2} = cowboy_req:body_qs(Req),
    case proplists:get_value(<<"user_id">>, Form) of
        undefined ->
            {false, Req2, State};
        UserIdStr ->
            User = tetris_user:get_user(binary_to_integer(UserIdStr)),
            lager:info("User ~p logged in.", [User]),
            {ok, Session} = tetris_http_session:set_value(user_id, User#user.id, maps:get(session, State)),
            {true, Req2, State#{user => User, session => Session}}
    end.

handle(Req, #{page := <<"login">>, user := undefined, method := <<"POST">>} = State) ->
    {LoggedIn, Req2, State2} = try_login(Req, State),
    case LoggedIn of
        true ->
            redirect(<<"/home">>, Req2, State2);
        false ->
            respond_template(login_dtl, #{errors => <<"Wrong login">>}, Req2, State2)
    end;
handle(Req, #{page := <<"login">>, user := undefined} = State) ->
    respond_template(login_dtl, [], Req, State);
handle(Req, #{user := undefined} = State) ->
    redirect(<<"/login">>, Req, State);
%% User is definitely authorized after previous clause
handle(Req, #{page := <<"login">>} = State) ->
    redirect(<<"/main">>, Req, State);
handle(Req, #{page := <<"game">>} = State) ->
    respond_template(game_dtl, [], Req, State);
handle(Req, #{page := Page} = State) when Page == undefined orelse Page == <<"main">> ->
    respond_template(main_dtl, [], Req, State);
handle(Req, State) ->
    lager:info("Req ~p", [State]),
    {ok, Req2} = cowboy_req:reply(404, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
