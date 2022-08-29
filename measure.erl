-module(measure).
-export([gen/2,time_insert/2,time/3]).

gen(_Module, 0) -> ok;
gen(Module, N) -> Module:insert({name, N}, getLocation(), getCompany()),
    gen(Module, N-1).

getLocation() -> lists:nth(rand:uniform(3), [home, work, school]).

getCompany() -> lists:nth(rand:uniform(3), [ericsson, nokia, motorola]).

time_insert(Module, N) ->
    {MicroSec, _} = time:tc(?MODULE, gen, [Module, N]),
    io:format("~s number: ~p insert=~p ms~n", [Module, N, MicroSec]).

time(M, F, A) ->
    {MicroSec, R} = time:tc(M, F, A),
    io:format("~s ~s ~p call=~p ms return ~p~n", [M, F, A, MicroSec, R]).