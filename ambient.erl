-module(ambient).
-import(grid, [create_grid/2, get_cell/3, set_cell/4, update_position/6]).
-export([init_world/2]).

init_world(W, H) when W > 0 , H > 0->
    register(ambient, self()),
    io:format("Ambient created with PID -> ~p\n", [self()]),
    Grid = create_grid(W, H),
    process_flag(trap_exit, true),
    world_listener(Grid, W, H)
    .

world_listener(Grid, W, H) ->
    io:format("##    ambient -> world_listener/1    ##\n"),
        receive         
        
        {isFree, PID, X, Y, Ref} ->
            NumberOfCars = get_cell(Grid, X, Y),
            ListOfParkedCar = lists:filter(fun({_, IsFree}) -> IsFree =/= true end, NumberOfCars),
            case length(ListOfParkedCar) of
                0 -> PID ! {status, Ref, true};
                1 -> PID ! {status, Ref, false}
            end,
            world_listener(Grid, W, H);

        {park, PID, X, Y, _} -> 
            NumberOfCars = get_cell(Grid, X, Y),
            ListOfParkedCar = lists:filter(fun({_, IsFree}) -> IsFree =/= true end, NumberOfCars), % if the park is NOT avilable
            case length(ListOfParkedCar) of
                0 -> 
                    MonitorRef = monitor(process, PID),
                    UpdatedGrid = update_position(Grid, X, Y, {MonitorRef, false}, W, H),
                    render ! {parked, PID, X, Y, true},
                    world_listener(UpdatedGrid, W, H);
                1 -> 
                    io:format("The car ~p has been killed by ambiend because in {~p, ~p} there is already a car parked.\n", [PID, X, Y]),
                    exit(PID, kill),
                    world_listener(Grid, W, H)
            end;

        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Ambient demonitoring car with PID -> ~p. Reason: ~p\n", [Pid, Reason]),
            demonitor(Ref),
            UpdatedGrid = maps:remove(Pid, Grid),
            world_listener(UpdatedGrid, W, H);

        {'EXIT', From, Reason} -> 
            io:format("Ambient received exit message from ~p with reason ~p\n", [From, Reason]),
            UpdatedGrid = maps:remove(From, Grid),
            world_listener(UpdatedGrid, W, H)
    end.