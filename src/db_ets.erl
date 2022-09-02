-module(db_ets).
-include_lib("db_record.hrl").
-export([db_init/0, db_put/3, db_get/2]).
-export([db_delete/2,db_close/1, db_query/2, db_empty/1]).

db_init() -> {ok, ets:new(my_ets, [set, protected, {keypos, #files.key}])}.

db_put(Key, Data, DbInfo) ->
    ets:insert(DbInfo, #files{key=Key, data=Data}),
    {ok, DbInfo}.

db_get(Key, DbInfo) ->
    Data = lists:append(ets:match(DbInfo, #files{key=Key, data='$2'})),
    case length(Data) == 0 of
        true -> undefined;
        false -> [H|_] = Data, {ok, H}
    end.

db_delete(Key, DbInfo) ->
    ets:delete(DbInfo, Key),
    {ok, DbInfo}.

db_close(_DbInfo) -> ok.

db_query(Data, DbInfo) ->
    KeyList = lists:append(ets:match(DbInfo, #files{key='$1', data=Data})),
    case length(KeyList) == 0 of
        true -> undefined;
        false -> {ok, KeyList}
    end.

db_empty(_DbInfo) ->
    {ok, ets:new(my_ets, [set, protected,{keypos, #files.key}])}.