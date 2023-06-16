-module(car).
-export().
-import(grid,[create_grid/2]).

main() -> 
    Pid_frindship = spawn_link(?MODULE, friendship, []),
    Pid_state = spawn_link(?MODULE, state, []),
    Pid_detect = spawn_link(?MODULE, detect, []).

friendship(StatePid, FRIENDLIST) when FRIENDLIST =:= [] -> 
    link(StatePid),
    Ref = make_ref(),
    % invia a StatePID il PID della friendship
    StatePid ! {friendshipPid, self()},
    % init della lista di amici richiedendola a wellknown
    wellknown ! {getFriends, self(), StatePid, Ref},
    receive
        {myFriends, PIDLIST, Ref} ->
            add_friendship(StatePid, FRIENDLIST, PIDLIST)
    end;

add_friendship(StatePid, FRIENDLIST, PIDLIST) ->
    case length(FRIENDLIST) < 5 of
        % add degli amici
        true ->
            case length(FRIENDLIST) > 0 of
                % nel caso ho almeno un amico, richiedo l'amicizia ai suoi amici
                true -> 
                    NewFriendList = ask_mutual_friend(StatePid, FRIENDLIST, []),
                    case length(FRIENDLIST) < 5 of
                        true -> 
                            {NewFriendList, NewPidList} = get_friend(StatePid, PIDLIST, FRIENDLIST),
                            add_friendship(StatePid, NewFriendList, NewPidList).
                        false ->
                            friendship(StatePid, NewFriendList)
                    end;
                            
                % non ho amici :c
                false -> 
                    {NewFriendList, NewPidList} = get_friend(StatePid, PIDLIST, FRIENDLIST),
                    add_friendship(StatePid, NewFriendList, NewPidList).
            end;

        false ->
            friendship(StatePid, FRIENDLIST)
    end;

% funzione di supporto per richiedere amicizia ad amici di amici
ask_mutual_friend(StatePid, FRIENDLIST, NewFriends) -> 
    {PID1, PID2} = lists::hd(FRIENDLIST),

    Ref = make_ref(),

    % init della lista di amici richiedendola a wellknown
    PID1 ! {getFriends, self(), StatePid, Ref},
    receive
        {myFriends, PIDLIST, Ref} ->
            add_friendship(StatePid, FRIENDLIST, PIDLIST).
    end;

    CombinedList = lists:uunion(FRIENDLIST, PIDLIST),
    EditList = lists:delete({self(), StatePid}, CombinedList),
    EditList.


get_friend(StatePid, PIDLIST, NewFriends) - > 
    case length(PIDLIST) > 0 of
        true ->
            {NewFriendFriendship, NewFriendState} = lists:hd(PIDLIST),
            case ({NewFriendFriendship, NewFriendState} =:= {self(), StatePid}) of
                true ->
                    get_friend(StatePid, lists:delete({self(), StatePid}, PIDLIST), NewFriends);
                false ->
                    {lists:uunion(NewFriends, [{NewFriendFriendship, NewFriendState}]), PIDLIST}
            end;
        false -> 
            {NewFriends, PIDLIST}
    end;



state({X,Y},{R,C}) -> 
    Grid = create_grid(R,C),
    receive
        {friendshipPid, PID} -> state(Grid,{X,Y},PID)
    end.

state(Grid, {X,Y}, FriendshipPid) -> 
    receive
        %%TODO
    end.

detect() -> .

move(StatePid, W, H, {Target_X, Target_Y}) -> 
    Ref = make_ref(),
    StatePid ! {getMyPosition, self(), Ref},
    receive 
        {myPosition, {X, Y}, Ref} ->
            case {X =:= Target_X, Y =:= Target_Y} of
                {true,true} ->
                    {X,Y};

                {true,false} ->
                    {X, (Y + moveY(Y, Target_Y, H)) rem (H)}

                {false,true} ->
                    {(X + moveX(X, Target_X, W)) rem (W), Y}

                {false,false} ->
                           Axis = rand:uniform(2),
                            case Axis of 
                                1 -> {(X + moveX(X, Target_X, W)) rem (W), Y};
                                2 -> {X, (Y + moveY(Y, Target_Y, H)) rem (H)}
                            end
            
            end
    end.

moveX(X, Target_X, W) ->
        Margin_R = abs(Target_X - ((X+1) rem W)), 
        Margin_L = abs(Target_X - ((X-1) rem W)),

        io:format("X: Margin_R: ~p, Margin_L: ~p~n", [Margin_R, Margin_L]),

        case Margin_R =< Margin_L of
            true -> 1;
            false -> -1
        end.

moveY(Y, Target_Y, H) ->
        Margin_T = abs(Target_Y - ((Y+1) rem H)), 
        Margin_B = abs(Target_Y - ((Y-1) rem H)),

        io:format("Y: Margin_T: ~p, Margin_B: ~p~n", [Margin_T, Margin_B]),

        case Margin_T =< Margin_B of
            true -> 1;
            false -> -1
        end.

% Sospende l'esecuzione per N ms.
sleep(N) -> receive after N -> ok end.