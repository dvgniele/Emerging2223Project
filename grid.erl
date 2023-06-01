-module(grid).
-export([create_grid/2, get_cell/3, set_cell/4]).

% initialize a grid R * C
create_grid(R, C) ->
    create_grid(R, C, #{}, 0, 0).

create_grid(R, C, Grid, CurrentRowIndex, CurrentColumnIndex) when CurrentRowIndex =:= R, CurrentColumnIndex =:= C ->
    Grid;

create_grid(R, C, Grid, IndexRow, IndexCol) ->
    NewGrid = maps:put({IndexRow, IndexCol}, 0, Grid),
    {NewRow, NewCol} = increment_position(R, C, IndexRow, IndexCol),
    create_grid(R, C, NewGrid, NewRow, NewCol).

increment_position(R, C, CurrentRowIndex, CurrentColumnIndex) ->
    if
        (CurrentColumnIndex + 1) == C, (CurrentRowIndex + 1) < R -> { CurrentRowIndex + 1, 0 };
        (CurrentColumnIndex + 1) < C, (CurrentRowIndex + 1) =< R -> { CurrentRowIndex, CurrentColumnIndex + 1 };
        %CurrentRowIndex == R -> {R, C};
        true -> { R, C }
    end.

% Get the value of a cell at the specified row and column
get_cell(Grid, Row, Col) ->
    maps:get({Row, Col}, Grid, 0).

% Set the value of a cell at the specified row and column
set_cell(Grid, Row, Col, Value) ->
    maps:put({Row, Col}, Value, Grid).