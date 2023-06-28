-module(grid).
-export([create_grid/2, get_cell/3, set_cell/4, update_position/6, handle_parked/6, handle_leave/4, update_position_ambient/6, check_neg/2, check_pos/2]).

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
        (CurrentColumnIndex + 1) < C, (CurrentRowIndex + 1) < R -> { CurrentRowIndex, CurrentColumnIndex + 1 };
        %CurrentRowIndex == R -> {R, C};
        true -> { R, C }
    end.

% Get the value of a cell at the specified row and column
get_cell(Grid, Row, Col) ->
    %io:format("##    Grid -> get_cell    ##\n"),
    maps:get({Row, Col}, Grid, []).

% Set the value of a cell at the specified row and column
set_cell(Grid, Row, Col, Value) ->
    %io:format("##    Grid -> set_cell    ##\n"),
    maps:put({Row, Col}, Value, Grid).

check_neg(CurrentValue, MaxValue) ->
    if
        CurrentValue < 0 -> MaxValue;
        CurrentValue >= 0 -> CurrentValue
    end.

check_pos(CurrentValue, MaxValue) ->
    if
        CurrentValue == MaxValue -> 0;
        CurrentValue < MaxValue -> CurrentValue
    end.

% (X, Y) -> [{PID, isFree}]
remove_old_position_ambient(Grid, Row, Col, {Pid, _, _ }, R, C) ->
    io:format("checking (~p ~p)", [Row, check_neg(((Col - 1) rem C), (C -1))]),
    SearchUpList = get_cell(Grid, Row, check_neg(((Col - 1) rem C), (C -1)) ),
    SearchUp = lists:filter(fun({Current_PID, _, _}) -> Current_PID =/= Pid end, SearchUpList),
    GridUp = set_cell(Grid, Row, check_neg(((Col - 1) rem C), (C -1)), SearchUp),
    io:format("checking (~p ~p)", [Row, check_pos(((Col + 1) rem C), C)]),
    SearchDownList = get_cell(GridUp, Row, check_pos(((Col + 1) rem C), C) ),
    SearchDown = lists:filter(fun({Current_PID, _, _}) -> Current_PID =/= Pid end, SearchDownList),
    GridDown = set_cell(GridUp, Row, check_pos(((Col + 1) rem C), C), SearchDown),
    io:format("checking (~p ~p)", [check_neg(((Row - 1) rem R), (R -1)), Col]),
    SearchLeftList = get_cell(GridDown, check_neg(((Row - 1) rem R), (R -1)), Col ),
    SearchLeft = lists:filter(fun({Current_PID, _, _}) -> Current_PID =/= Pid end, SearchLeftList),
    GridRight = set_cell(GridDown, check_neg(((Row - 1) rem R), (R -1)), Col, SearchLeft),
    io:format("checking (~p ~p)", [check_pos(((Row + 1) rem R), R), Col]),
    SearchRightList = get_cell(GridRight, check_pos(((Row + 1) rem R), R), Col ),
    SearchRight = lists:filter(fun({Current_PID, _, _}) -> Current_PID =/= Pid end, SearchRightList),
    GridLeft = set_cell(GridRight, check_pos(((Row + 1) rem R), R), Col, SearchRight),
    SearchCurrentList = get_cell(GridLeft, Row, Col ),
    SearchCurrent = lists:filter(fun({Current_PID, _, _}) -> Current_PID =/= Pid end, SearchCurrentList),
    GridFinal = set_cell(GridLeft, Row, Col, SearchCurrent),
    GridFinal.

remove_old_all_position(Grid, Row, Col, { Pid, IsFree }, R, C) when Col < C ->
    CurrentList = get_cell(Grid, Row, Col),
    Search = lists:filter(fun({Current_PID, _}) -> Current_PID =/= Pid end, CurrentList),
    UpdatedGrid = set_cell(Grid, Row, Col, Search),
    remove_old_all_position(UpdatedGrid, Row, Col + 1, { Pid, IsFree }, R, C);

remove_old_all_position(Grid, Row, Col, {Pid, IsFree }, R, C) when Col == C, Row + 1 < R ->
    remove_old_all_position(Grid, Row + 1, 0, { Pid, IsFree }, R, C);

remove_old_all_position(Grid, Row, Col, _, R, C) when Col == C, Row + 1 == R ->
    Grid.

update_position(Grid, Row, Col, { Pid, IsFree }, R, C) ->
    %io:format("##    Grid -> update_position    ##\n"),
    GridWithoutOldPid = remove_old_all_position(Grid, 0, 0, { Pid, IsFree }, R, C),
    OldItemsWithoutPid = get_cell(Grid, Row, Col),
    NewItems = [ {  Pid, IsFree } ],
    UnionList = lists:usort(OldItemsWithoutPid ++ NewItems),
    NewGrid = set_cell(GridWithoutOldPid, Row, Col, UnionList),
    NewGrid.

update_position_ambient(Grid, Row, Col, { Pid, Ref, IsFree }, R, C) ->
    %io:format("##    Grid -> update_position    ##\n"),
    GridWithoutOldPid = remove_old_position_ambient(Grid, Row, Col, { Pid, Ref, IsFree }, R, C),
    OldItemsWithoutPid = get_cell(Grid, Row, Col),
    NewItems = [ {  Pid, Ref, IsFree } ],
    UnionList = lists:usort(OldItemsWithoutPid ++ NewItems),
    NewGrid = set_cell(GridWithoutOldPid, Row, Col, UnionList),
    NewGrid.

handle_parked(Grid, Row, Col, { Pid, IsFree }, R, C) ->
    CurrentItem = get_cell(Grid, Row, Col),
    CurrentItemWithoutPid = lists:filter(fun({Current_PID, _}) -> Current_PID =/= Pid end, CurrentItem),
    NewItems = [ {  Pid, IsFree } ],
    UnionList = lists:usort(CurrentItemWithoutPid ++ NewItems),
    NewGrid = set_cell(Grid, Row, Col, UnionList),
    NewGrid.

handle_leave(Grid, Pid, R, C) ->
    Result = handle_leave_custom(Grid, Pid, 0, 0, R, C),
    Result.

handle_leave_custom(Grid, Pid, Current_Row, Current_Column, R, C) when Current_Column < C ->
    CurrentList = get_cell(Grid, Current_Row, Current_Column),
    Search = lists:filter(fun({Current_PID, _, _}) -> Current_PID =:= Pid end, CurrentList),
    io:format("(~p, ~p) -> ~p\n", [Current_Row, Current_Column, Search]),
    case length(Search) of
        0 -> handle_leave_custom(Grid, Pid, Current_Row, Current_Column + 1, R, C);
        _ -> UpdatedList = lists:filter(fun({Current_PID, _, _}) -> Current_PID =/= Pid end, CurrentList),
                NewGrid = set_cell(Grid, Current_Row, Current_Column, UpdatedList),
                { NewGrid, Search, {Current_Row, Current_Column} }
    end;

handle_leave_custom(Grid, Pid, Current_Row, Current_Column, R, C) when Current_Column == C, Current_Row + 1 < R ->
    handle_leave_custom(Grid, Pid, Current_Row + 1, 0, R, C);

handle_leave_custom(Grid, Pid, Current_Row, Current_Column, R, C) when Current_Column == C, Current_Row + 1 == R ->
    {Grid, [], {}}.

