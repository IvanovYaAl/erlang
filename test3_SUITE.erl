-module(test3_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    db_list_test,
    db_ets_test,
    lr_test,
%% NOTE: The test below should be run manually to check the correctness of BS behaviour
    bs_test
  ].

log_start(FuncName)  -> ct:print("========== Start ~p ==========", [FuncName]).
log_finish(FuncName) -> ct:print("========== Finish ~p ==========", [FuncName]).

db_list_test(_) ->
  log_start(?FUNCTION_NAME),
  backend_test(db_list),
  backend_dup_test(db_list),
  log_finish(?FUNCTION_NAME).

db_ets_test(_) ->
  log_start(?FUNCTION_NAME),
  backend_test(db_ets),
  backend_dup_test(db_ets),
  log_finish(?FUNCTION_NAME).

backend_test(Module) ->
  ct:print("Init DB", []),
  {ok, D1} = Module:db_init(),

  ct:print("Put to DB", []),
  {ok, D2} = Module:db_put(key, data, D1),
  {ok, D3} = Module:db_put(key2, data, D2),
  {ok, D4} = Module:db_put(key3, [1, 2, 3], D3),

  ct:print("Get from DB", []),
  {ok, data} = Module:db_get(key, D4),
  {ok, data} = Module:db_get(key2, D4),
  {ok, [1,2,3]} = Module:db_get(key3, D4),

  ct:print("Query data from DB", []),
  {ok, Query} = Module:db_query(data, D4),
  ?assertEqual(2, length(Query)),
  ?assert(lists:member(key, Query)),
  ?assert(lists:member(key2, Query)),

  ct:print("Delete from DB", []),
  {ok, D5} = Module:db_delete(key, D4),

  ct:print("Get unexisted key from DB", []),
  undefined = Module:db_get(key, D5),

  ct:print("Query unexisted data from DB", []),
  undefined = Module:db_query(unknown_data, D5),

  ct:print("Empty DB", []),
  {ok, D6} = Module:db_empty(D5),
  undefined = Module:db_get(key, D6),
  undefined = Module:db_get(key2, D6),
  undefined = Module:db_get(key3, D6),

  ct:print("Close DB", []),
  ok = Module:db_close(D6).

backend_dup_test(Module) ->
  OldData = data,
  {ok, D1} = Module:db_init(),
  {ok, D2} = Module:db_put(key, OldData, D1),
  ct:print("Put data with the same key to DB", []),
  {ok, D3} = Module:db_put(key, newdata, D2),
  {ok, Data} = Module:db_get(key, D3),
  ct:print("Data is changed from ~p to ~p", [OldData, Data]),
  {ok, _} = Module:db_empty(D3),
  ok = Module:db_close(D3).

lr_test(_) ->
  log_start(?FUNCTION_NAME),

  ct:print("Start LR", []),
  {ok, _Pid} = lr:start_link(),

  ct:print("Where is unknown MS?", []),
  ?assertEqual(lost, lr:where_is(unknown_ms)),

  ct:print("Who are at unknown BS?", []),
  ?assertEqual(none, lr:who_are_at(unknown_bs)),

  ct:print("Locate some MS to some BS", []),
  ?assertEqual(ok, lr:located_at(ms1, bs1)),
  ?assertEqual(ok, lr:located_at(ms2, bs2)),
  ?assertEqual(ok, lr:located_at(ms3, bs1)),
  ?assertEqual(ok, lr:located_at(ms4, bs2)),

  ct:print("Locate duplicated MS", []),
  ?assertEqual(ok, lr:located_at(ms4, bs2)),

  ct:print("Where is MS?", []),
  ?assertEqual({ok, bs1}, lr:where_is(ms1)),
  ?assertEqual({ok, bs2}, lr:where_is(ms2)),
  ?assertEqual({ok, bs1}, lr:where_is(ms3)),
  ?assertEqual({ok, bs2}, lr:where_is(ms4)),

  ct:print("Who are at BS?", []),
  {ok, AtBs1} = lr:who_are_at(bs1),
  {ok, AtBs2} = lr:who_are_at(bs2),
  ?assert(lists:member(ms1, AtBs1)),
  ?assert(lists:member(ms3, AtBs1)),
  ?assert(lists:member(ms2, AtBs2)),
  ?assert(lists:member(ms4, AtBs2)),
  ?assertEqual(2, length(AtBs1)),
  ?assertEqual(2, length(AtBs2)),

  ct:print("Lost MS", []),
  ?assertEqual(ok, lr:lost(ms1)),
  ?assertEqual(ok, lr:lost(ms1)),
  ?assertEqual(lost, lr:where_is(ms1)),
  ?assertEqual({ok, [ms3]}, lr:who_are_at(bs1)),

  ?assertEqual(ok, lr:lost(ms2)),
  ?assertEqual(lost, lr:where_is(ms2)),
  ?assertEqual({ok, [ms4]}, lr:who_are_at(bs2)),

  ct:print("Empty LR", []),
  ?assertEqual(ok, lr:empty()),
  ?assertEqual(lost, lr:where_is(ms3)),
  ?assertEqual(lost, lr:where_is(ms4)),
  ?assertEqual(none, lr:who_are_at(bs1)),
  ?assertEqual(none, lr:who_are_at(bs2)),

  ct:print("Stop LR", []),
  ?assertEqual(ok, lr:stop()),

  log_finish(?FUNCTION_NAME).

bs_test(_) ->
  log_start(?FUNCTION_NAME),

  ct:print("Start BS & LR", []),
  lr:start_link(),
  {ok, BsPid} = bs:start_link(bs1),
  sys:trace(BsPid, true),

  ct:print("Start several MS", []),
  ms:start (ms1, BsPid),
  ms:start (ms2, BsPid),
  timer:sleep(5000),

  ct:print("Stop BS", []),
  ?assertEqual(ok, bs:stop(BsPid)),
  timer:sleep(5000),

  log_finish(?FUNCTION_NAME).
