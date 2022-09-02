-module(bs).
-behaviour(gen_statem).

-import(db_ets, [db_init/0, db_close/1, db_put/3,
    db_get/2, db_delete/2, db_empty/1, db_query/2]).

-export([init/1, handle_event/3,
    handle_sync_event/4, handle_info/3, terminate/3]).

-export([start_link/1, stop/1, connect/3, ack/2]).

-export([callback_mode/0]).

-export([available/2, available/3, get_ack/2, get_ack/3,
        full/3, full/2, poll/2, info/1]).

-define(SERVER, ?MODULE). 
-define(rec_info(T,R),lists:zip(record_info(fields,T), tl(tuple_to_list(R)))).

-record(state, {name, ms_pid, ms_name, current_capacity=0, timer_ref}).


start_link(BSName) ->
    gen_statem:start_link({local, BSName}, ?MODULE, BSName, []).

stop(BSPid) ->
    gen_statem:stop(BSPid).

connect(BSPid, MSPid, MSName) ->
    gen_statem:send_event(BSPid, {connect, MSPid, MSName}).

ack(BSPid,MSName) ->
    gen_statem:send_event(BSPid, {acknowledgement,MSName}).

info(BSPid) -> gen_statem:sync_send_event(BSPid, info,5000).

init(BSName) ->
    {ok, PollDB} = db_init(),
    case timer:send_interval(5000, self(), {trigger, PollDB}) of
        {ok, TRef} -> {ok, available, #state{name=BSName,timer_ref=TRef}};
        {error, _Reason} ->
            bs:stop(BSName)
    end.

available(info, _From, State=#state{}) ->
    {reply, ok, available, State}.

available({connect, MSPid, MSName}, State=#state{}) ->
    case State#state.current_capacity < 5 of
        true -> phone:respond(connect,MSName),
            {next_state, get_ack, State#state{ms_pid = MSPid, ms_name = MSName}};
        false -> phone:respond(reject,MSName),
            {next_state, available, State}
    end;
available(timeout, State=#state{}) ->
    {next_state, available, State};

available(_Event, State=#state{}) ->
    {next_state, available, State}.

get_ack(info, _From, State=#state{}) ->
    {reply, ok, get_ack, State}.

get_ack({acknowledgement, MSName}, State = #state{}) ->
    lr:located_at(MSName, State#state.name),
    {next_state, available, State};

get_ack(timeout, State=#state{}) ->
    {next_state, available, State};

get_ack(_Event, State) ->
    {next_state, acknowledgement, State}.

full(timeout, State=#state{}) ->
    {next_state, full, State}.

full(info, _From, State=#state{}) ->
    {reply, ok, full, State};

full({connect, _MSPid, MSName}, _From, State=#state{}) ->
    phone:respond(reject,MSName),
    {next_state, full, State};

full(timeout, _From, State=#state{}) ->
    {next_state, get_ack, State#state{ms_pid = nopid, ms_name = noname}};

full(_Event, _From, State=#state{}) ->
    {next_state, available, State}.

handle_event(_Event, StateName, State = #state{}) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State = #state{}) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

poll(MSName, PollDB) ->
    case db_get(MSName,PollDB) of
        undefined  -> case erlang:whereis(MSName) of
                          undefined ->
                              db_put(MSName, 1, PollDB);
                          _Pid ->
                              db_put(MSName, 0, PollDB)
                      end;
        {ok, Attempt} -> case lists:last(Attempt) < 5 of
                             true -> 
                                 case erlang:whereis(MSName) of
                                     undefined ->
                                         db_put(MSName, lists:last(Attempt) + 1, PollDB);
                                     _Pid ->
                                         db_put(MSName, 0, PollDB)
                                 end;
                             false -> 
                                 lr:lost(MSName),
                                 db_delete(MSName, PollDB)
                         end;
        Other -> io:format("~p ~p: unexpected msg ~n", [MSName,Other])
    end.


handle_info({trigger, PollDB}, StateName, State = #state{}) ->
    case whereis(lr) of
        undefined -> io:format("~p: lr is not started ~n", [State#state.name]),
            {next_state, StateName, State};
        _Pid -> case lr:who_are_at(State#state.name) of
                    none ->
                        case StateName of
                            full -> 
                                {next_state, available, State};
                            _Other -> 
                                {next_state, StateName, State}
                        end;
                    {ok,PhoneList} -> 
                        case length(PhoneList) < 5  of
                            true -> [poll(Elem,PollDB) || Elem <- PhoneList],
                                case StateName of
                                    full -> 
                                        {next_state, available, State#state{current_capacity = length(PhoneList)}};
                                    _Other -> 
                                        {next_state, StateName,  State#state{current_capacity = length(PhoneList)}}
                                end;
                            false ->
                                case StateName of
                                    full -> 
                                        [poll(Elem,PollDB) || Elem <- PhoneList],
                                        {next_state, full, State#state{current_capacity = length(PhoneList)}};
                                    _Other -> 
                                        [poll(Elem,PollDB) || Elem <- PhoneList],
                                        {next_state, full,  State#state{current_capacity = length(PhoneList)}}
                                end
                        end
                end
    end;

handle_info(_Info, StateName, State = #state{}) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State = #state{}) ->
    ok.

callback_mode() ->
    handle_event_function.