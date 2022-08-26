-module(test2_SUITE).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    db_list_test,
    db_record_test,
    db_ets_test,
    db_dets_test,
    funs_test,
    lc_test,
    db_distr_test,
    db_distr_robust_test
  ].

-define(SERVER_NODE_NAME, server).

log_start(FuncName)  -> ct:print("========== Start ~p ==========", [FuncName]).
log_finish(FuncName) -> ct:print("========== Finish ~p ==========", [FuncName]).

db_list_test(_) ->
  log_start(?FUNCTION_NAME),

  ct:print("Start server"),
  ?assertEqual(ok, db_list:start()),

  ct:print("DB is empty. Retrieve all names, all locations"),
  ?assertEqual([], db_list:all_names()),
  ?assertEqual([], db_list:all_locations()),

  ct:print("Insert name to the DB"),
  ?assertEqual(ok, db_list:insert("Alex", "NN")),

  ct:print("Insert the same name to the DB (already inserted)"),
  ?assertEqual(already_inserted, db_list:insert("Alex", "NN")),
  ?assertEqual(already_inserted, db_list:insert("Alex", "Dzerzhinsk")),

  ct:print("Insert more names to the DB"),
  ?assertEqual(ok, db_list:insert("Vanya", "NN")),
  ?assertEqual(ok, db_list:insert("Liza", "Dzerzhinsk")),
  ?assertEqual(ok, db_list:insert("Eva", "Dzerzhinsk")),

  ct:print("Retrieve location by name from the DB"),
  ?assertEqual("NN", db_list:where_is("Alex")),
  ?assertEqual("NN", db_list:where_is("Vanya")),
  ?assertEqual("Dzerzhinsk", db_list:where_is("Liza")),
  ?assertEqual("Dzerzhinsk", db_list:where_is("Eva")),

  ct:print("Remove existed name from the DB"),
  ?assertEqual(ok, db_list:remove("Eva")),
  ?assertEqual({no_such_name, "Eva"}, db_list:where_is("Eva")),

  ct:print("Remove unexisted name from the DB"),
  ?assertEqual({no_such_name, "Unknown"}, db_list:where_is("Unknown")),
  ?assertEqual(ok, db_list:remove("Unknown")),

  ct:print("Insert some more names to the DB"),
  ?assertEqual(ok, db_list:insert("Eva", "Moscow")),
  ?assertEqual("Moscow", db_list:where_is("Eva")),
  ?assertEqual(ok, db_list:insert("Lyuba", "NN")),
  ?assertEqual("NN", db_list:where_is("Lyuba")),

  ct:print("Retrieve names by location from the DB"),
  NamesNN = db_list:located_at("NN"),
  ?assert(lists:member("Alex", NamesNN)),
  ?assert(lists:member("Vanya", NamesNN)),
  ?assert(lists:member("Lyuba", NamesNN)),
  ?assertEqual(3, length(NamesNN)),
  ?assertEqual(["Liza"], db_list:located_at("Dzerzhinsk")),
  ?assertEqual(["Eva"], db_list:located_at("Moscow")),

  ct:print("Retrieve by unexisted location"),
  ?assertEqual(none, db_list:located_at("London")),

  ct:print("DB is not empty. Retrieve all names, all locations"),
  ?assertEqual(["Alex", "Eva", "Liza", "Lyuba", "Vanya"], db_list:all_names()),
  ?assertEqual(["Dzerzhinsk", "Moscow", "NN"], db_list:all_locations()),

  ct:print("Stop server"),
  ?assertEqual(server_terminated, db_list:stop()),
  log_finish(?FUNCTION_NAME).

db_record_test(_) ->
  log_start(?FUNCTION_NAME),
  db_test(db_record),
  log_finish(?FUNCTION_NAME).

db_ets_test(_) ->
  log_start(?FUNCTION_NAME),
  db_test(db_ets),
  log_finish(?FUNCTION_NAME).

db_dets_test(_) ->
  log_start(?FUNCTION_NAME),
  db_test(db_dets),
  log_finish(?FUNCTION_NAME).

% to test db_record/db_ets/db_dets/db_distr
db_test(Module) ->
  case Module of
    db_distr -> skip;
    _ ->
      ct:print("Start server"),
      ?assertEqual(ok, Module:start())
  end,

  ct:print("DB is empty. Retrieve all names, all locations"),
  ?assertEqual([], Module:all_names()),
  ?assertEqual([], Module:all_locations()),

  ct:print("Insert name to the DB"),
  ?assertEqual(ok, Module:insert("Alex", "NN", "Intel")),

  ct:print("Insert the same name to the DB (already inserted)"),
  ?assertEqual(already_inserted, Module:insert("Alex", "NN", "Intel")),
  ?assertEqual(already_inserted, Module:insert("Alex", "Dzerzhinsk", "Intel")),

  ct:print("Insert more names to the DB"),
  ?assertEqual(ok, Module:insert("Vanya", "NN", "Intel")),
  ?assertEqual(ok, Module:insert("Liza", "Dzerzhinsk", "Orion")),
  ?assertEqual(ok, Module:insert("Eva", "Dzerzhinsk", "Orion")),

  ct:print("Retrieve location by name from the DB"),
  ?assertEqual("NN", Module:where_is("Alex")),
  ?assertEqual("NN", Module:where_is("Vanya")),
  ?assertEqual("Dzerzhinsk", Module:where_is("Liza")),
  ?assertEqual("Dzerzhinsk", Module:where_is("Eva")),

  ct:print("Remove existed name from the DB"),
  ?assertEqual(ok, Module:remove("Eva")),
  ?assertEqual({no_such_name, "Eva"}, Module:where_is("Eva")),

  ct:print("Remove unexisted name from the DB"),
  ?assertEqual({no_such_name, "Unknown"}, Module:where_is("Unknown")),
  ?assertEqual(ok, Module:remove("Unknown")),

  ct:print("Insert some more names to the DB"),
  ?assertEqual(ok, Module:insert("Eva", "Moscow", "Intel")),
  ?assertEqual("Moscow", Module:where_is("Eva")),
  ?assertEqual(ok, Module:insert("Lyuba", "NN", "Orion")),
  ?assertEqual("NN", Module:where_is("Lyuba")),

  ct:print("Retrieve names by location from the DB"),
  NamesNN = Module:located_at("NN"),
  ?assert(lists:member("Alex", NamesNN)),
  ?assert(lists:member("Vanya", NamesNN)),
  ?assert(lists:member("Lyuba", NamesNN)),
  ?assertEqual(3, length(NamesNN)),
  ?assertEqual(["Liza"], Module:located_at("Dzerzhinsk")),
  ?assertEqual(["Eva"], Module:located_at("Moscow")),

  ct:print("Retrieve by unexisted location"),
  ?assertEqual(none, Module:located_at("London")),

  ct:print("DB is not empty. Retrieve all names, all locations"),
  ?assertEqual(["Alex", "Eva", "Liza", "Lyuba", "Vanya"], Module:all_names()),
  ?assertEqual(["Dzerzhinsk", "Moscow", "NN"], Module:all_locations()),

  ct:print("Retrieve names by company from the DB"),
  NamesIntel = Module:working_at("Intel"),
  ?assert(lists:member("Alex", NamesIntel)),
  ?assert(lists:member("Vanya", NamesIntel)),
  ?assert(lists:member("Eva", NamesIntel)),
  ?assertEqual(3, length(NamesIntel)),
  NamesOrion = Module:working_at("Orion"),
  ?assert(lists:member("Lyuba", NamesOrion)),
  ?assert(lists:member("Liza", NamesOrion)),
  ?assertEqual(2, length(NamesOrion)),

  ct:print("Retrieve by unexisted company"),
  ?assertEqual(none, Module:working_at("Mera")),

  case Module of
    db_distr -> skip;
    _ ->
      ct:print("Stop server"),
      ?assertEqual(server_terminated, Module:stop())
  end.

funs_test(_) ->
  log_start(?FUNCTION_NAME),
  ?assertEqual([0,3,5], funs:llen([[], [1,2,3], [a,v,d,s,s]])),

  ?assertEqual(1, funs:min([2,3,4,5,1,6,7])),

  ?assertEqual(true, funs:rail(fun (X) -> X == 1 end, [1, 1, 1])),
  ?assertEqual(false, funs:rail(fun (X) -> X == 1 end, [1, 2, 1])),

  ?assertEqual(false, funs:all_even([1, 2, 3, 4])),
  ?assertEqual(true, funs:all_even([2, 4, 6])),

  ?assertEqual(5, funs:first(fun (X) -> X > 3 end, [1, 3, 5, 7])),
  ?assertEqual(false, funs:first(fun (X) -> X > 30 end, [1, 3, 5, 7])),

  Add5 = funs:mk_adder(5),
  ?assertEqual(10, Add5(5)),
  ?assertEqual(8, Add5(3)),
  log_finish(?FUNCTION_NAME).

lc_test(_) ->
  log_start(?FUNCTION_NAME),
  ?assertEqual([ernie, burt], lc:atoms([{hello, 1}, 3, ernie, [a, b, c], burt])),
  ?assertEqual([], lc:atoms([{hello, 1}, 3, [a, b, c], 4])),

  ?assertEqual([3, ernie, burt], lc:atoms_ints([{hello, 1}, 3, ernie, [a, b, c], burt])),
  ?assertEqual([], lc:atoms_ints([{hello, 1}, [a, b, c]])),

  ?assertEqual([{b,2}], lc:keysearch(b, [{a, 1},{b, 2},{c, 3}])),
  ?assertEqual([], lc:keysearch(x, [{a, 1},{b, 2},{c, 3}])),

  ?assertEqual([2,4,6], lc:join([1,2,3,4,5,6], [2,4,6,8,10,12])),
  ?assertEqual([], lc:join([1,2,3], [4,6,8])),

  ?assertEqual([1,3,5], lc:diff([1,2,3,4,5,6], [2,4,6,8,10,12])),
  ?assertEqual([], lc:diff([1,2,3,4,5,6], [6,5,4,3,2,1])),
  log_finish(?FUNCTION_NAME).

%%%%%%%%%% Distributed Erlang test %%%%%%%%%%

start_slave_node(NodeName) ->
  CodePath = code:get_path(),
  ErlFlags = "-pa " ++ lists:concat(lists:join(" ", CodePath)),
  ResultNode = case ct_slave:start(NodeName,
    [{kill_if_fail, true},
      {monitor_master, true},
      {init_timeout, 3000},
      {startup_timeout, 3000},
      {startup_functions,
        [{db_distr, start, []}]},
      {erl_flags, ErlFlags}]) of
    {ok, Node} ->
      Node;
    {error, already_started, Node} ->
      stop_slave_node(NodeName),
      ct_slave:start(NodeName,
        [{kill_if_fail, true},
          {monitor_master, true},
          {init_timeout, 3000},
          {startup_timeout, 3000},
          {startup_functions,
            [{db_distr, start, []}]},
          {erl_flags, ErlFlags}]),
      Node;
      {error, started_not_connected, Node} ->
        stop_slave_node(NodeName),
        ct_slave:start(NodeName,
          [{kill_if_fail, true},
            {monitor_master, true},
            {init_timeout, 3000},
            {startup_timeout, 3000},
            {startup_functions,
              [{db_distr, start, []}]},
            {erl_flags, ErlFlags}]),
        Node;
    UnexpectedError -> ct:fail("Failed to start remote node with error ~p", [UnexpectedError])
  end,
  ResultNode.

stop_slave_node(NodeName) ->
  ct_slave:stop(NodeName).

check_connection_to_db() ->
  Res = db_distr:all_names(),
  {ok, HostName} = inet:gethostname(),
  case Res of
    [] ->
      ct:print("db_distr:all_names() returned '~p'. DB is reachable", [Res]);
    _ ->
      ct:print("db_distr:all_names() returned '~p'. DB is NOT reachable. "
               "Make sure you node name is [server + @ + inet:gethostname()], i.e. [server@~s]. "
               "It should be aligned between DB and CT.", [Res, HostName]),
      ct:fail("DB is NOT reachable")

  end.

db_distr_test(_) ->
  log_start(?FUNCTION_NAME),

  ServerNodeName = ?SERVER_NODE_NAME,
  ct:print("\e[32m ServerNodeName is [~p] \e[0m", [ServerNodeName]),

  ct:print("\e[32m Prepare server node to start DB \e[0m"),
  ServerNode = start_slave_node(ServerNodeName),

  ct:print("\e[32m Node ~p [OK] \e[0m", [ServerNode]),
  pong = net_adm:ping(ServerNode),

  ct:print("\e[32m Trying to reach DB from node ~p to node ~p \e[0m", [node(), ServerNode]),
  check_connection_to_db(),

  ct:print("\e[32m Run DB distr test on client node\e[0m"),
  db_test(db_distr),

  ct:print("\e[32m Stop server node\e[0m"),
  timer:sleep(1000),
  stop_slave_node(ServerNodeName),

  log_finish(?FUNCTION_NAME).

db_distr_robust_test(_) ->
  log_start(?FUNCTION_NAME),

  ServerNodeName = ?SERVER_NODE_NAME,
  ct:print("\e[32m ServerNodeName is [~p] \e[0m", [ServerNodeName]),

  ct:print("\e[32m Check DB distr is not crashed/hanged if node does not exists\e[0m"),
  DbRes1 = db_distr:all_names(),
  ct:print("\e[32m DB response is ~p \e[0m", [DbRes1]),

  ct:print("\e[32m Prepare server node to start DB \e[0m"),
  ServerNode = start_slave_node(ServerNodeName),
 
  ct:print("\e[32m Node ~p [OK] \e[0m", [ServerNode]),
  pong = net_adm:ping(ServerNode),

  ct:print("\e[32m Trying to reach DB from node ~p to node ~p \e[0m", [node(), ServerNode]),
  check_connection_to_db(),

  ct:print("\e[32m Stop DB on server node\e[0m"),
  Res = rpc:call(ServerNode, db_distr, stop, []),
  ct:print("\e[32m DB is stopped ~p\e[0m", [Res]),

  ct:print("\e[32m Check DB distr is not crashed/hanged if DB is not started\e[0m"),
  DbRes2 = db_distr:all_names(),
  ct:print("\e[32m DB response is ~p \e[0m", [DbRes2]),

  ct:print("\e[32m Stop server node\e[0m"),
  timer:sleep(1000),
  stop_slave_node(ServerNodeName),

  ct:print("\e[32m Check DB distr is not crashed/hanged if node does not exists\e[0m"),
  DbRes3 = db_distr:all_names(),
  ct:print("\e[32m DB response is ~p \e[0m", [DbRes3]),

  log_finish(?FUNCTION_NAME).
