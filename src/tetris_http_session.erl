-module(tetris_http_session).

-compile([export_all]).

-export([create_tables/0]).
-export([ensure_session/1, set_value/3]).

-record(session, {session_id :: binary(),
                  attributes = #{} :: #{}}).

create_tables() ->
    {atomic, ok} = mnesia:create_table(http_sessions,
                                       [{attributes, record_info(fields, session)},
                                        {disc_copies, [node()]},
                                        {record_name, session}]).

generate_session_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

new_session() ->
    NewSession = #session{session_id = generate_session_id()},
    mnesia:dirty_write(http_sessions, NewSession),
    NewSession.

get_session(SessionId) ->
    case mnesia:dirty_read(http_sessions, SessionId) of 
        [] ->
            [];
        [#session{attributes = #{}} = Session] ->
            [Session];
        _ ->
            []
    end.

set_session_cookie(Req, Session) ->
    cowboy_req:set_resp_cookie(<<"session_id">>, Session#session.session_id, [], Req).

ensure_session(Req) ->
    case cowboy_req:cookie(<<"session_id">>, Req) of
        {undefined, Req2} ->
            NewSession = new_session(),
            {ok, NewSession, set_session_cookie(Req2, NewSession)};
        {SessionId, Req2} ->
            case get_session(SessionId) of
                [] ->
                    NewSession = new_session(),
                    {ok, NewSession, set_session_cookie(Req2, NewSession)};
                [Session] ->
                    {ok, Session, Req2}
            end
    end.

set_value(Session, Name, Value) ->
    Session2 = Session#session{attributes = maps:put(Name, Value, Session#session.attributes)},
    mnesia:dirty_write(http_sessions, Session2),
    {ok, Session2}.

get_value(Key, #session{attributes = Attributes}) ->
    maps:get(Key, Attributes).
