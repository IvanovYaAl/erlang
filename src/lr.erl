-module(lr).
-behaviour(gen_server).

-import(db_ets, [db_init/0, db_close/1, db_put/3,
    db_get/2, db_delete/2, db_empty/1, db_query/2]).

-export([start_link/0, located_at/2, lost/1, where_is/1]).
-export([who_are_at/1, empty/0, stop/0, init/1]).

-export([handle_call/3, handle_cast/2, handle_info/2,
    terminate/2]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

located_at(MS, BS) ->
    gen_server:call(?MODULE, {located_at, MS, BS}).

lost(MS) ->
    gen_server:call(?MODULE, {lost, MS}).

where_is(MS) ->
    gen_server:call(?MODULE, {where_is, MS}).

who_are_at(BS) ->
    gen_server:call(?MODULE, {who_are_at, BS}).

empty() ->
    gen_server:call(?MODULE, empty).

stop() ->
    gen_server:call(?MODULE, stop),
    gen_server:stop(?MODULE).

init([]) ->
    db_init().

handle_call({where_is, MS}, _From, DbInfo) ->
    Reply = case db_get(MS, DbInfo) of
        undefined -> lost;
        {ok, BS} -> {ok, BS}
    end,
    {reply, Reply, DbInfo};

handle_call({located_at, MS, BS}, _From, DbInfo) ->
    {ok, NewDbInfo} = db_put(MS, BS, DbInfo),
    Reply = ok,
    {reply, Reply, NewDbInfo};

handle_call({lost, MS}, _From, DbInfo) ->
    {ok, NewDbInfo} = db_delete(MS, DbInfo),
    Reply = ok,
    {reply, Reply, NewDbInfo};

handle_call({who_are_at, BS}, _From, DbInfo) ->
    Reply = case db_query(BS, DbInfo) of
                undefined -> none;
                {ok, MSList} -> {ok, MSList}
            end,
    {reply, Reply, DbInfo};

handle_call(empty, _From, DbInfo) ->
    {ok, NewDbInfo} = db_empty(DbInfo),
    Reply = ok,
    {reply, Reply, NewDbInfo};

handle_call(stop, _From, DbInfo) ->
    db_close(DbInfo),
    {reply, ok, []}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
   