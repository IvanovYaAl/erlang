-module(test1_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    demo_test,
    temp_test,
    mathStuff_test,
    lists1_test,
    time_test,
%% NOTE: The tests below possibly requires interface change in test exercise.
%% NOTE: The tests below should be run manually to check the correctness of message sending/receiving.
    two_proc_test,
    ring_test,
    star_test,
    ms_test,
    two_proc_test_boundary,
    ring_test_boundary,
    star_test_boundary,
    ms_test_boundary
  ].

log_start(FuncName)  -> ct:print("========== Start ~p ==========", [FuncName]).
log_finish(FuncName) -> ct:print("========== Finish ~p ==========", [FuncName]).

swedish_year(Y) -> lists:nthtail(2, integer_to_list(Y)).
swedish_month_day(X) when X > 9 -> integer_to_list(X);
swedish_month_day(X) -> "0" ++ integer_to_list(X).
swedish_date_ref() ->
  {Y, M, D} = date(),
  swedish_year(Y) ++ swedish_month_day(M) ++ swedish_month_day(D).

demo_test(_) ->
  log_start(?FUNCTION_NAME),
  ?assertEqual(demo:double(5), 10),
  log_finish(?FUNCTION_NAME).

temp_test(_) ->
  log_start(?FUNCTION_NAME),
  ?assert(temp:f2c(248) == 120),
  ?assert(temp:f2c(-4) == -20),
  ?assert(temp:c2f(120) == 248),
  ?assert(temp:c2f(-20) == -4),
  ?assert(temp:convert({f, 248}) == {c, 120}),
  ?assert(temp:convert({f, -4})  == {c, -20}),
  ?assert(temp:convert({c, 120}) == {f, 248}),
  ?assert(temp:convert({c, -20}) == {f, -4}),
  ?assert(temp:convert({f, 72.5}) == {c, 22.5}),
  ?assert(temp:convert({c, 22.5}) == {f, 72.5}),
  log_finish(?FUNCTION_NAME).

mathStuff_test(_) ->
  log_start(?FUNCTION_NAME),
  ?assertEqual(mathStuff:perimeter({square, 3}), 12),
  ?assertEqual(mathStuff:perimeter({circle, 10}), 20 * math:pi()),
  ?assertEqual(mathStuff:perimeter({triangle, 3, 4, 5}), 12),
  log_finish(?FUNCTION_NAME).

lists1_test(_) ->
  log_start(?FUNCTION_NAME),
  ?assertEqual(lists1:min([5]), 5),
  ?assertEqual(lists1:max([5]), 5),
  ?assertEqual(lists1:min_max([5]), {5, 5}),
  ?assertEqual(lists1:min([1,2,-5,-4,4,10,2,5,11,2,0]), -5),
  ?assertEqual(lists1:max([1,2,-5,-4,4,10,2,5,11,2,0]), 11),
  ?assertEqual(lists1:min_max([1,2,-5,-4,4,10,2,5,11,2,0]), {-5, 11}),
  ?assertEqual(lists1:min([1,2,-5,-4,4,10,2,5,11,2,0,-6]), -6),
  ?assertEqual(lists1:max([1,2,-5,-4,4,10,2,5,11,2,0,15]), 15),
  ?assertEqual(lists1:min_max([1,2,-5,-4,4,10,2,5,11,2,0,-6]), {-6, 11}),
  log_finish(?FUNCTION_NAME).

time_test(_) ->
  log_start(?FUNCTION_NAME),
  ?assertEqual(time:swedish_date(), swedish_date_ref()),
  log_finish(?FUNCTION_NAME).

two_proc_test(_) ->
  log_start(?FUNCTION_NAME),
  two_proc_test_common(3),
  log_finish(?FUNCTION_NAME).

two_proc_test_boundary(_) ->
  log_start(?FUNCTION_NAME),
  two_proc_test_common(1),
  log_finish(?FUNCTION_NAME).

two_proc_test_common(NumOfMsg) ->
  log_start(?FUNCTION_NAME),
  log_start(lists:flatten(io_lib:format("NumOfMsg = ~p", [NumOfMsg]))),

  case code:ensure_loaded(two_proc) of
    {error, _What} ->
      ct:fail("Please rename module as 'two_proc' to proceed the test");
    _ ->
      Exported1 = erlang:function_exported(two_proc, start, 1),
      Exported2 = erlang:function_exported(two_proc, send_msg, 4),
      case Exported1 and Exported2 of
        true -> ok;
        false ->
          ct:print("!!!!!!!!!! Please change the interface to following: !!!!!!!!!!~n"
          "two_proc:start(NumOfMsg) -> {Pid1, Pid2}~n"
          "two_proc:send_msg(Pid1, Pid2, MessageString, NumOfMsg) -> ok", []),
          ct:fail("Please change the interface of 'two_proc' module to proceed the test")
      end
  end,

  ct:print("Start 2 processes"),
  {Sender, Receiver} = two_proc:start(NumOfMsg),
  ?assertEqual(true, is_process_alive(Sender)),
  ?assertEqual(true, is_process_alive(Receiver)),

  ct:print("Test 2 processes exchange ~p message(s)", [NumOfMsg]),
  two_proc:send_msg(Sender, Receiver, "My msg", NumOfMsg),
  timer:sleep(1000),

  ct:print("Check all processes are terminated after all messages have been sent"),
  ?assertEqual(false, is_process_alive(Sender)),
  ?assertEqual(false, is_process_alive(Receiver)),
  log_finish(?FUNCTION_NAME).

ring_test(_) ->
  log_start(?FUNCTION_NAME),
  ring_test_common(5, 3),
  log_finish(?FUNCTION_NAME).

ring_test_boundary(_) ->
  log_start(?FUNCTION_NAME),
  ring_test_common(1, 1),
  log_finish(?FUNCTION_NAME).

ring_test_common(NumOfProc, NumOfMsg) ->
  log_start(?FUNCTION_NAME),

  case code:ensure_loaded(ring) of
    {error, _What} ->
      ct:fail("Please rename module as 'ring' to proceed the test");
    _ ->
      Exported1 = erlang:function_exported(ring, start, 1),
      Exported2 = erlang:function_exported(ring, send_msg, 3),
      case Exported1 and Exported2 of
        true -> ok;
        false ->
          ct:print("!!!!!!!!!! Please change the interface to following: !!!!!!!!!!~n"
          "ring:start(NumOfProc) -> [Pids]~n"
          "ring:send_msg([Pids], MessageString, NumOfMsg) -> ok", []),
          ct:fail("Please change the interface of 'ring' module to proceed the test")
      end
  end,

  ct:print("Start ~p processes", [NumOfProc]),
  Pids = ring:start(NumOfProc),
  lists:foreach(
    fun(Pid) -> ?assertEqual(true, is_process_alive(Pid)) end, Pids),

  ct:print("Test ~p processes exchange ~p messages in a ring. Pids: ~p", [NumOfProc, NumOfMsg, Pids]),
  ring:send_msg(Pids, "Ring of power", NumOfMsg),
  timer:sleep(1000),

  ct:print("Check all processes are terminated after all messages have been sent"),
  lists:foreach(
    fun(Pid) -> ?assertEqual(false, is_process_alive(Pid)) end, Pids),
  log_finish(?FUNCTION_NAME).

star_test(_) ->
  log_start(?FUNCTION_NAME),
  star_test_common(5, 3),
  log_finish(?FUNCTION_NAME).

star_test_boundary(_) ->
  log_start(?FUNCTION_NAME),
  star_test_common(2, 1),
  log_finish(?FUNCTION_NAME).

star_test_common(NumOfProc, NumOfMsg) ->
  log_start(?FUNCTION_NAME),

  case code:ensure_loaded(star) of
    {error, _What} ->
      ct:fail("Please rename module as 'star' to proceed the test");
    _ ->
      Exported1 = erlang:function_exported(star, start, 1),
      Exported2 = erlang:function_exported(star, send_msg, 4),
      case Exported1 and Exported2 of
        true -> ok;
        false ->
          ct:print("!!!!!!!!!! Please change the interface to following: !!!!!!!!!!~n"
          "star:start(NumOfProc) -> {CenterPid, [OtherPids]}~n"
          "star:send_msg(CenterPid, [OtherPids], MessageString, NumOfMsg) -> ok", []),
          ct:fail("Please change the interface of 'star' module to proceed the test")
      end
  end,

  ct:print("Start ~p processes in a star topology", [NumOfProc]),
  {Center, Pids} = star:start(NumOfProc),
  lists:foreach(
    fun(Pid) -> ?assertEqual(true, is_process_alive(Pid)) end, Pids),
  ct:print("Test ~p processes exchange ~p messages in a star. Center: ~p, Pids: ~p", [NumOfProc, NumOfMsg, Center, Pids]),
  star:send_msg(Center, Pids, "Super", NumOfMsg),
  timer:sleep(1000),

  ct:print("Check all processes are terminated after all messages have been sent"),
  ?assertEqual(false, is_process_alive(Center)),
  lists:foreach(
    fun(Pid) -> ?assertEqual(false, is_process_alive(Pid)) end, Pids),
  log_finish(?FUNCTION_NAME).

ms_test(_) ->
  log_start(?FUNCTION_NAME),
  ms_test_common(5),
  log_finish(?FUNCTION_NAME).

ms_test_boundary(_) ->
  log_start(?FUNCTION_NAME),
  ms_test_common(1),
  log_finish(?FUNCTION_NAME).

ms_test_common(N) ->
  log_start(?FUNCTION_NAME),

  ct:print("Start the master and ~p slaves", [N]),
  ms:start(N),
  ?assertNotEqual(undefined, whereis(master)),

  ct:print("Send the message from master to slaves"),
  [ms:to_slave("Hi, slave!", Index) || Index <- lists:seq(1, N)],
  timer:sleep(1000),

  case N < 2 of
    true ->
      skip_test_step;
    false ->
      ct:print("Restart one slave with 'die' message"),
      ms:to_slave(die, 2),
      timer:sleep(1000),

      ct:print("Send the message again to check the slave is restarted"),
      [ms:to_slave("Hi, slave!", Index) || Index <- lists:seq(1, N)],
      timer:sleep(1000)
  end,

  ct:print("Restart all slaves with 'die' message"),
  [ms:to_slave(die, Index) || Index <- lists:seq(1, N)],
  timer:sleep(1000),

  ct:print("Send the message again to check the slaves are restarted"),
  [ms:to_slave("Hi, slave!", Index) || Index <- lists:seq(1, N)],
  timer:sleep(1000),

  ct:print("Kill the master to cleanup"),
  exit(whereis(master), kill),
  timer:sleep(1000),
  ?assertEqual(undefined, whereis(master)),
  log_finish(?FUNCTION_NAME).
