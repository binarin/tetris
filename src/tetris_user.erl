-module(tetris_user).
-compile([{parse_transform, lager_transform}]).

-include("tetris_user.hrl").

-export([create_tables/0]).
-export([get_user/1, to_proplist/1]).

create_tables() ->
    {atomic, ok} = mnesia:create_table(user,
                                       [{attributes, record_info(fields, user)},
                                        {disc_copies, [node()]}]).

get_user(UserId) when is_integer(UserId) ->
    [UserRecord] = mnesia:dirty_read(user, UserId),
    UserRecord.

to_proplist(#user{id = Id, attr = Attr}) ->
    [{id, Id}, {attr, Attr}].
