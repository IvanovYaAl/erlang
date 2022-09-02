-module(phone).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([respond/2,stop/1,location/2,connect/1,info/1]).

-define(SERVER, ?MODULE).
-define(rec_info(T,R),lists:zip(record_info(fields,T),tl(tuple_to_list(R)))).

-record(phone_state, {ms_pid, ms_name, ms_state=on, status=disconnected, bs_name=undefined}).


start_link(MSName) ->
    gen_server:start_link({local, MSName}, ?MODULE, MSName, []).

respond(Request, MSPid) -> gen_server:call(MSPid, {respond, Request}).

location(BSName, MSPid) -> gen_server:call(MSPid, {location,BSName}).

connect(MSPid) -> gen_server:call(MSPid, connect).

info(MSPid) -> gen_server:call(MSPid, info).

stop(MSPid) ->
    gen_server:call(MSPid, stop),
    gen_server:stop(MSPid).

init(MSName) -> {ok,#phone_state{ms_pid=self(),ms_name=MSName}}.

handle_call({respond,connect}, _From, State=#phone_state{}) ->
    bs:ack(State#phone_state.bs_name,State#phone_state.ms_name),
    {reply, ok, State#phone_state{status = connected}};

handle_call({respond,reject}, _From, State=#phone_state{}) ->
    {reply, reject, State#phone_state{status = disconnected}};

handle_call({respond,busy}, _From, State=#phone_state{}) ->
    {reply, busy, State#phone_state{status = disconnected}};

handle_call({location,BSName}, _From, State=#phone_state{}) ->
    Reply = ok,
    {reply, Reply, State#phone_state{bs_name = BSName}};

handle_call(connect, _From, State = #phone_state{}) ->
    case State#phone_state.bs_name of
        undefined -> {reply, {error, undefined_bs_name}, State};
        BSName -> case erlang:whereis(BSName) of
                      undefined ->
                          {reply, {error, bs_is_not_started, BSName}, State};
                      _Pid ->
                          bs:connect(BSName, State#phone_state.ms_pid, State#phone_state.ms_name),
                          {reply, ok, State}
                  end
    end;

handle_call(info, _From, State=#phone_state{}) ->
    {reply, ok, State};

handle_call(stop, _From, State=#phone_state{}) ->
    Reply = ok,
    {reply, Reply, State#phone_state{ms_state=off}};

handle_call(Request, _From, State=#phone_state{}) ->
    Reply = {error,unknown_request,Request},
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.