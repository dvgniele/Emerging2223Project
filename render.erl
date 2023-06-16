-module(render).
-import(grid,[create_grid/2]).
-export([render/2]).

render_loop() ->
    sleep(1000),
    render ! {draw},
    render_loop().

render(W, H) ->
    register(render, self()),
    io:format("##    Render: actor registered as 'render' with pid ~p    ##\n", [self()])
    spawn_link(?MODULE, render_loop, []),
    Grid = create_grid(W,H),
    render(Grid, W, H).

render(CarsPositionsMap, W, H) ->
    
    receive
        {draw} -> 
            io:format("weooofraaa"),
            render(CarsPositionsMap, W, H)
    end.

% Stampa lo stato attuale della grid
draw() -> .

% Sospende l'esecuzione per N ms
sleep(N) -> receive after N -> ok end.