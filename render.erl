-module(render).
-import(grid,[create_grid/2, update_position/6, get_cell/3, handle_parked/6]).
-export([render/2, render_loop/0]).

render_loop() ->
    io:format("##    render -> render_loop/0    ##\n"),
    sleep(1000),
    render ! {draw},
    render_loop().

render(W, H) ->
    %io:format("##    render -> render/2    ##\n"),
    register(render, self()),
    io:format("##    Render: actor registered as 'render' with pid ~p    ##\n", [self()]),
    spawn_link(?MODULE, render_loop, []),
    Grid = create_grid(W,H),
    render(Grid, W, H).

render(Grid, W, H) ->
    %io:format("##    render -> render/3    ##\n"),
    % ogni cella mantine la lista delle automobili che sono su di essa (X, Y) -> [{PID, isFree}]
    receive
        % update the position of the car
        {position, PID, X, Y} ->
            monitor(process, PID),
            NewGrid = update_position(Grid, X, Y, { PID, false }, W, H),
            render(NewGrid, W, H);
        % print new target for PID car
        {target, PID, X, Y} ->
            io:format("Car ~p wants to park in {~p, ~p}\n", [PID, X, Y]),
            render(Grid, W, H);
        % update the status of the car (parked or not)
        {parked, PID, X, Y, IsParked} -> 
            monitor(process, PID),
            NewCarsPositionsMap = handle_parked(Grid, X, Y, { PID, IsParked }, W, H),
            render(NewCarsPositionsMap, W, H);
        % print the new friendlist
        {friends, PID, PIDLIST} ->
            %io:format("Car ~p new friend list:\n~p\n", [PID,PIDLIST]),
            render(Grid, W, H);
        {'DOWN', Ref, process, Pid, _} ->
            demonitor(Ref),
            NewGrid = maps:remove(Pid, Grid),
            render(NewGrid, W, H);
        {draw} -> 
            io:format("\n\n~p\n\n", [Grid]),
            draw(Grid, W, H),
            render(Grid, W, H)
    end.

% Stampa lo stato attuale della grid

draw(Grid, W, H) ->
    draw(Grid, W, H, 0, 0)
    .

draw(Grid, W, H, PosX, PosY) when PosY < H ->
    CurrentCars = get_cell(Grid, PosX, PosY),
    case length(CurrentCars) of
        0 -> io:format(" #");
        1 -> 
            {Pid, IsFree} = hd(CurrentCars),
            case IsFree of 
                true -> io:format(" $");
                false -> io:format(" X")
            end;
        _ -> io:format(" M")
    end,
    draw(Grid, W, H, PosX, PosY + 1);

% % increase the row and start to printing again from column 0
draw(Grid, W, H, PosX, PosY) when PosX + 1 < W, PosY == H -> 
    io:format("\n"),
    draw(Grid, W, H, PosX + 1, 0);

draw(_, W, H, PosX, PosY) when PosX + 1 == W, PosY == H -> 
     io:format("\n\n\n Legenda: \n # -> no car \n X -> one car \n $ -> parked car \n M -> many car \n", []).

% % print the current row
% draw(Grid, W, H, PosX, PosY) when PosY + 1 < H -> 
%     io:format("##    render -> draw/5    ##\n"),
%     CarsList = get_cell(Grid, PosX, PosY),
%     io:format("(~p  ~p) -> ~p\n", [PosX, PosY, length(CarsList)]),
%     draw(Grid, W, H, PosX, PosY + 1);

% % increase the row and start to printing again from column 0
% draw(Grid, W, H, PosX, PosY) when PosX + 1 < W, PosY + 1 == H -> 
%     io:format("##    render -> draw/5 (2)   ##\n"),
%    draw(Grid, W, H, PosX + 1, 0);

% draw(_, W, H, PosX, PosY) when PosX + 1 == W, PosY + 1 == H -> 
%     io:format("End draw\n", []).

% Sospende l'esecuzione per N ms
sleep(N) -> receive after N -> ok end.