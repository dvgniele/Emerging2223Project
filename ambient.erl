-module(ambient).
-export([init_world/0]).
-import(grid, [create_grid/2, get_cell/3, set_cell/4]).

init_world() ->
    register(ambient, self()),
    N = 50,
    M = 50,
    Grid = create_grid(N, M),
    world_listener(Grid)
    .

world_listener(Grid) -> .