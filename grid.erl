-module(grid).
-export([create_grid/2, get_cell/3, set_cell/4, update_position/6]).

% initialize a grid R * C
create_grid(R, C) ->
    %io:format("##    Grid -> create_grid/2    ##\n"),
    create_grid(R, C, #{}, 0, 0).

create_grid(R, C, Grid, CurrentRowIndex, CurrentColumnIndex) when CurrentRowIndex =:= R, CurrentColumnIndex =:= C ->
    %io:format("##    Grid -> create_grid/5    ##\n"),
    Grid;

create_grid(R, C, Grid, IndexRow, IndexCol) ->
    %io:format("##    Grid -> create_grid/5 (2)   ##\n"),
    NewGrid = maps:put({IndexRow, IndexCol}, [], Grid),
    {NewRow, NewCol} = increment_position(R, C, IndexRow, IndexCol),
    create_grid(R, C, NewGrid, NewRow, NewCol).

increment_position(R, C, CurrentRowIndex, CurrentColumnIndex) ->
    %io:format("##    Grid -> increment_position    ##\n"),
    if
        (CurrentColumnIndex + 1) == C, (CurrentRowIndex + 1) < R -> { CurrentRowIndex + 1, 0 };
        (CurrentColumnIndex + 1) < C, (CurrentRowIndex + 1) =< R -> { CurrentRowIndex, CurrentColumnIndex + 1 };
        %CurrentRowIndex == R -> {R, C};
        true -> { R, C }
    end.

% Get the value of a cell at the specified row and column
get_cell(Grid, Row, Col) ->
    io:format("##    Grid -> get_cell    ##\n"),
    maps:get({Row, Col}, Grid, []).

% Set the value of a cell at the specified row and column
set_cell(Grid, Row, Col, Value) ->
    io:format("##    Grid -> set_cell    ##\n"),
    maps:put({Row, Col}, Value, Grid).

% (X, Y) -> [{PID, isFree}]

remove_old_position(Grid, Row, Col, {Pid, _ }, R, C) ->
    %io:format("##    Grid -> remove_old_position    ##\n"),
    SearchUpList = get_cell(Grid, Row, (Col - 1) rem C),
    SearchUp = lists:filter(fun({Current_PID, _}) -> Current_PID =/= Pid end, SearchUpList),
    set_cell(Grid, Row, (Col - 1) rem C, SearchUp),
    SearchDownList = get_cell(Grid, Row, (Col + 1) rem C ),
    SearchDown = lists:filter(fun({Current_PID, _}) -> Current_PID =/= Pid end, SearchDownList),
    set_cell(Grid, Row, (Col + 1) rem C, SearchDown),
    SearchLeftList = get_cell(Grid, (Row - 1) rem R, Col ),
    SearchLeft = lists:filter(fun({Current_PID, _}) -> Current_PID =/= Pid end, SearchLeftList),
    set_cell(Grid, (Row - 1) rem R, Col, SearchLeft),
    SearchRightList = get_cell(Grid, (Row + 1) rem R, Col ),
    SearchRight = lists:filter(fun({Current_PID, _}) -> Current_PID =/= Pid end, SearchRightList),
    set_cell(Grid, (Row + 1) rem R, Col, SearchRight),
    SearchCurrentList = get_cell(Grid, Row, Col ),
    SearchCurrent = lists:filter(fun({Current_PID, _}) -> Current_PID =/= Pid end, SearchCurrentList),
    set_cell(Grid, Row, Col, SearchCurrent),
    Grid.

update_position(Grid, Row, Col, { Pid, IsFree }, R, C) ->
    %io:format("##    Grid -> update_position    ##\n"),
    GridWithoutOldPid = remove_old_position(Grid, Row, Col, { Pid, IsFree }, R, C),
    OldItemsWithoutPid = get_cell(grid, Row, Col),
    NewItems = [ {  Pid, IsFree } ],
    UnionList = lists:uunion(OldItemsWithoutPid, NewItems),
    NewGrid = set_cell(GridWithoutOldPid, Row, Col, UnionList),
    NewGrid.
