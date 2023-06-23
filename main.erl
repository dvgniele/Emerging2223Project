-module(main).
-export([init/0]).
%-import(ambient, [init_world/2]).
%-import(car, [main/4]).

init() -> 
    io:format("##    Main -> Starting    ##\n"),
    W = 10, H = 10,
    NCars = 10,
    NCarsToPurge = 3,
    SleepInterval = 10000,
    spawn(ambient, init_world, [W, H]),
    spawn(wellknown, wellknown, []),
    spawn(render, render, [W, H]),
    create_cars(NCars, [], W, H, SleepInterval, NCarsToPurge)
    .

create_cars(NCars, CarsPids, W, H, SleepInterval, NCarsToPurge) when NCars > 0 ->
    io:format("##    Main -> Creating Cars\n"),
    X = rand:uniform(W),
    Y = rand:uniform(H),
    CarPid = spawn(car, main, [X, Y, W, H]),
    NewPids = [CarPid | CarsPids],
    create_cars(NCars - 1, NewPids, W, H, SleepInterval, NCarsToPurge);


create_cars(NCars, CarsPids, W, H, SleepInterval, NCarsToPurge) when NCars =:= 0 ->
    %io:format("##    Main -> Create_cars/6    ##\n"),
    io:format("Cars List: ~p\n", [CarsPids]),
    sleep(SleepInterval),
    cars_purge().


cars_purge() -> 
    io:format("##    Cars Purge    ##").

%%% sleep/1
% Sospende l'esecuzione per N ms
sleep(N) -> receive after N -> ok end.