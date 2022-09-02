-module(db_list).
-include_lib("db_record.hrl").
-export([db_init/0, db_put/3, db_get/2]).
-export([db_delete/2,db_close/1, db_query/2, db_empty/1]).

db_init() -> {ok, []}.

db_put(Key, Data, DbInfo) ->
    {ok, DeletedInfo} = db_delete(Key, DbInfo),
    NewDbInfo = lists:append(DeletedInfo, [#files{key=Key, data=Data}]),
    {ok, NewDbInfo}.

db_get(Key, DbInfo) ->
    case lists:keyfind(Key, #files.key, DbInfo) of
        #files{data=Data} -> {ok, Data};
        false -> undefined;
        Other_ -> ct:print("Other info ~p~n", [Other_])
    end.

db_delete(Key, DbInfo) ->
    NewDbInfo = lists:keydelete(Key, #files.key, DbInfo),
    {ok, NewDbInfo}.

db_close(_DbInfo) -> ok.

db_query(Data, DbInfo) ->
    KeyList = [Key || #files{key=Key, data=DataDbInfo} <- DbInfo, DataDbInfo == Data],
    case length(KeyList) == 0 of
        false -> {ok, KeyList};
        true -> undefined
    end.

db_empty(_DbInfo) -> {ok, []}.