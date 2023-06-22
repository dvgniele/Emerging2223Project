-module(car).
-import(grid, [create_grid/2, get_cell/3]).
-import(lists, [hd/1, delete/2, uunion/2]).
-export([main/4, friendship/2, detect/4, state/2]).

main(X, Y, W, H) -> 
    io:format("##    Car -> main/4    ##\n"),
    io:format("## Car -> Starting ##\n"),
    link(whereis(ambient)),
    Pid_state = spawn_link(?MODULE, state, [{X, Y}, {W, H}]),
    Pid_friendship = spawn_link(?MODULE, friendship, [Pid_state, []]),
    spawn_link(?MODULE, detect, [Pid_state, Pid_friendship, W, H]),
    process_flag(trap_exit, true),
    main(whereis(ambient), W, H).

main(AmbientPid, W, H) ->
    io:format("##    Car -> main/3    ##\n"),
    receive
        {'EXIT', AmbientPid, Reason} -> 
            io:format("Ambient is dead. Reason: ~p. Car ~p is killing itself\n", [Reason, self()]),
            exit(kill);
        {'EXIT', From, Reason} -> 
            io:format("Main received exit message from ~p with reason ~p\n", [From, Reason]),
            NewX = rand:uniform(W),
            NewY = rand:uniform(H),
            main(NewX, NewY, W, H)
    end.

friendship(StatePid, FRIENDLIST) when FRIENDLIST =:= [] -> 
    io:format("##    Car -> friendship/2    ##\n"),
    link(StatePid),
    Ref = make_ref(),
    % invia a StatePID il PID della friendship
    StatePid ! {friendshipPid, self()},
    % init della lista di amici richiedendola a wellknown
    wellknown ! {getFriends, self(), StatePid, Ref},
    receive
        {myFriends, PIDLIST, Ref} ->
            add_friendship(StatePid, FRIENDLIST, PIDLIST)
    end.

add_friendship(StatePid, FRIENDLIST, PIDLIST) ->
    io:format("##    Car -> add_friendship/3    ##\n"),
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
                            add_friendship(StatePid, NewFriendList, NewPidList);
                        false ->
                            friendship(StatePid, NewFriendList)
                    end;
                            
                % non ho amici :c
                false -> 
                    {NewFriendList, NewPidList} = get_friend(StatePid, PIDLIST, FRIENDLIST),
                    add_friendship(StatePid, NewFriendList, NewPidList)
            end
    end.

% funzione di supporto per richiedere amicizia ad amici di amici
ask_mutual_friend(StatePid, FRIENDLIST, NewFriends) -> 
    io:format("##    Car -> ask_mutual_friend/3    ##\n"),
    {PID1, PID2} = lists:hd(FRIENDLIST),

    Ref = make_ref(),

    % init della lista di amici richiedendola a wellknown
    PID1 ! {getFriends, self(), StatePid, Ref},
    receive
        {myFriends, PIDLIST, Ref} ->
            add_friendship(StatePid, FRIENDLIST, PIDLIST)
    end,

    CombinedList = lists:uunion(FRIENDLIST, PIDLIST),
    EditList = lists:delete({self(), StatePid}, CombinedList),
    EditList.


get_friend(StatePid, PIDLIST, NewFriends) -> 
    io:format("##    Car -> get_friend/3    ##\n"),
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
    end.

%% detect/4
detect(StatePid, FriendshipPid, W, H) ->
    io:format("##    Car -> detect/4    ##\n"),
    link(StatePid),
    link(FriendshipPid),
    detect(StatePid, W, H).

%% detect/3
detect(StatePid, W, H) ->
    io:format("##    Car -> detect/3    ##\n"),
    TargetX = rand:uniform(W),
    TargetY = rand:uniform(H),
    NewTargetRef = make_ref(),
    StatePid ! {newTarget, self(), {TargetX, TargetY}, NewTargetRef},
    receive
        {targetFree, IsTargetFree, NewTargetRef} ->
            case IsTargetFree of
                true -> 
                    % Notifico render sulla scelta del nuovo posteggio obiettivo
                    render ! {target, StatePid, TargetX, TargetY},
                    detect(StatePid, W, H, TargetX, TargetY);
                false -> detect(StatePid, W, H)
            end
    end.

%% detect/5
detect(StatePid, W, H, TargetX, TargetY) ->
    io:format("##    Car -> detect/5    ##\n"),
    IsTargetFreeRef = make_ref(),
    StatePid ! {isTargetFree, self(), IsTargetFreeRef},
    receive
        {targetFree, IsTargetFree, IsTargetFreeRef} ->
            case IsTargetFree of
                true ->
                    io:format("DETECT -> RIGA 132\n")
                    % {NewX, NewY} = move(StatePid, W, H, TargetX, TargetY),
                    % % Aggiorno il render sulla mia nuova posizione
                    % render ! {position, StatePid, NewX, NewY},
                    % IsFreeRef = make_ref(),
                    % ambient ! {isFree, self(), NewX, NewY, IsFreeRef},
                    % receive
                    %             %  - {status, Ref, IsFree} è la risposta da parte dell'ambiente
                    %             % all'attore il cui PID PID era contenuto nella richiesta.
                    %             % Il booleano IsFree vale true sse il posteggio è libero.
                    %     {status, IsFreeRef, IsFree} ->
                    %                 % In seguito alla ricezione del messaggio status, il messaggio viene
                    %                 % condiviso con l'attore "state" tramite un protocollo privato.
                    %         StatePid ! {status, {NewX, NewY}, IsFree},
                    %                 % Nel caso in cui sia stato raggiunto il posteggio obiettivo e questo sia libero:
                    %                 %  - {park, PID, X, Y, Ref} viene invato all'attore "ambient" per dire
                    %                 %    che l'automobile sta parcheggiando. Ref è una nuova reference.
                    %         case (NewX =:= TargetX) and (NewY =:= TargetY) of
                    %             true ->
                    %                 case IsFree of 
                    %                     true ->
                    %                         ParkRef = make_ref(),
                    %                         ambient ! {park, StatePid, NewX, NewY, ParkRef},
                    %                                 % Notifica status che la cella è ora occupata (in modo da poterlo condividere durante il gossiping).
                    %                         StatePid ! {status, self(), {NewX, NewY}, false},
                    %                                 %  - {leave, PID, Ref} viene inviato dopo 1-5s (valore scelto casualmente) all'attore
                    %                                 % "ambient" per dire che l'automobile sta lasciando il posteggio. La reference contenuta
                    %                                 % nel messaggio deve essere identica a quella del messaggio precedente.
                    %                         sleep(rand:uniform(5000)),
                    %                         ambient ! {leave, StatePid, ParkRef},
                    %                         detect(StatePid, W, H);
                    %                     false ->
                    %                         detect(StatePid,  W, H);
                                            
                    %                     false ->
                    %                         sleep(2000),
                    %                         detect(StatePid, W, H, TargetX, TargetY)
                    %                 end
                    %         end
                    % end
            end
    end.

move(StatePid, W, H, Target_X, Target_Y) -> 
    io:format("##    Car -> move/5    ##\n"),
    Ref = make_ref(),
    StatePid ! {getMyPosition, self(), Ref},
    receive 
        {myPosition, {X, Y}, Ref} ->
            case {X =:= Target_X, Y =:= Target_Y} of
                {true,true} ->
                    {X,Y};

                {true,false} ->
                    {X, (Y + moveY(Y, Target_Y, H)) rem (H)};

                {false,true} ->
                    {(X + moveX(X, Target_X, W)) rem (W), Y};
                
                {false,false} ->
                           Axis = rand:uniform(2),
                            case Axis of 
                                1 -> {(X + moveX(X, Target_X, W)) rem (W), Y};
                                2 -> {X, (Y + moveY(Y, Target_Y, H)) rem (H)}
                            end
            
            end
    end.

moveX(X, Target_X, W) ->
    io:format("##    Car -> moveX    ##\n"),
        Margin_R = abs(Target_X - ((X+1) rem W)), 
        Margin_L = abs(Target_X - ((X-1) rem W)),

        io:format("X: Margin_R: ~p, Margin_L: ~p~n", [Margin_R, Margin_L]),

        case Margin_R =< Margin_L of
            true -> 1;
            false -> -1
        end.

moveY(Y, Target_Y, H) ->
    io:format("##    Car -> moveY    ##\n"),
        Margin_T = abs(Target_Y - ((Y+1) rem H)), 
        Margin_B = abs(Target_Y - ((Y-1) rem H)),

        io:format("Y: Margin_T: ~p, Margin_B: ~p~n", [Margin_T, Margin_B]),

        case Margin_T =< Margin_B of
            true -> 1;
            false -> -1
        end.

state({ X, Y }, { W, H } ) -> 
    io:format("##    Starting State/2    ##\n"),
    Grid = create_grid(W,H),
    receive
        {friendshipPid, PID} -> state(Grid, {X, Y}, PID)
    end.

%% state/3
state(Grid, {X, Y}, FriendshipPid) ->
    io:format("##    Starting State/3    ##\n"),
    receive
        {newTarget, PID, {TargetX, TargetY}, Ref} ->
            is_target_free(PID, Ref, Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid)
    end.

%% state/4
state(Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid) ->
    io:format("##    Starting State/4    ##\n"),
    receive
        % Richiesta da parte di "detect" sulla posizione dell'automobile sulla scacchiera.
        {getMyPosition, PID, Ref} -> 
            PID ! {myPosition, {X, Y}, Ref},
            state(Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid);
        % Richiesta da parte di "detect" di aggiornare la posizione dell'automobile con "{NewX, NewY}".
        {updateMyPosition, {NewX, NewY}} -> 
            state(Grid, {NewX, NewY}, {TargetX, TargetY}, FriendshipPid);
        % Richiesta da parte di "detect" di aggiornare la cella obiettivo con "{NewTargetX, NewTargetY}".
        {newTarget, PID, {NewTargetX, NewTargetY}, Ref} ->
            is_target_free(PID, Ref, Grid, {X, Y}, {NewTargetX, NewTargetY}, FriendshipPid);
        % Richiesta da parte di "detect" se la cella obiettivo è libera o meno.
        {isTargetFree, PID, Ref} ->
            is_target_free(PID, Ref, Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid);
        % Notifica da parte di "detect" sullo stato della cella su cui si trova l'automobile.
        {status, {X, Y}, IsFree} ->
            status_update({X, Y}, IsFree, Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid);
        {notifyStatus, {X, Y}, IsFree} ->
            status_update({X, Y}, IsFree, Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid)
    end.

%% status_update/6
status_update({X, Y}, IsFree, Grid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid) ->
    io:format("##    Car -> status_update/6    ##\n"),
    % Questa informazione modifica l'ambiente interno?
    NumberOfCars = get_cell(Grid, X, Y),
    ListOfParkedCar = lists:filter(fun({_, IsFreeInside}) -> IsFreeInside =/= true end, NumberOfCars),
    case length(ListOfParkedCar) of
        0 ->
            case IsFree  of
                true -> state(Grid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid);
                false ->
                    % Chiede a friendship la lista dei suoi amici e invia notifyStatus ad ognuno di loro (gossiping).
                    GetFriendsListRef = make_ref(),
                    FriendshipPid ! {getFriendsList, GetFriendsListRef},
                    receive
                        {friendsList, FRIENDSLIST, GetFriendsListRef} ->
                            notify_friends(FRIENDSLIST, {X, Y}, IsFree, Grid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid)
                    end
            end;
        _ ->
            case IsFree  of
                false -> state(Grid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid);
                true ->
                    % Chiede a friendship la lista dei suoi amici e invia notifyStatus ad ognuno di loro (gossiping).
                    GetFriendsListRef = make_ref(),
                    FriendshipPid ! {getFriendsList, GetFriendsListRef},
                    receive
                        {friendsList, FRIENDSLIST, GetFriendsListRef} ->
                            notify_friends(FRIENDSLIST, {X, Y}, IsFree, Grid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid)
                    end
            end
    end.

%% notify_friends/7
notify_friends(FRIENDSLIST, {X, Y}, IsFree, Grid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid) ->
    io:format("##    Car -> notify_friends/7    ##\n"),
    case length(FRIENDSLIST) > 0 of
        true ->
            lists:last(tuple_to_list(lists:last(FRIENDSLIST))) ! {notifyStatus, {X, Y}, IsFree},
            notify_friends(lists:droplast(FRIENDSLIST), {X, Y}, IsFree, Grid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid);
        false ->
            NewGrid = maps:put({X, Y}, IsFree, Grid),
            state(NewGrid, {PosX, PosY}, {TargetX, TargetY}, FriendshipPid)
    end.

%% is_target_free/6
is_target_free(DetectPid, Ref, Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid) ->
    io:format("##    Car -> is_target_free/6    ##\n"),
    % Controlla se la cella è libera .
    NumberOfCars = get_cell(Grid, X, Y),
    ListOfParkedCars = lists:filter(fun({_CarId, IsFree}) -> IsFree =/= true end, NumberOfCars),
    case length(ListOfParkedCars) of
        0 -> DetectPid ! {targetFree, true, Ref}, state(Grid, {X, Y}, {TargetX, TargetY}, FriendshipPid);
        _ ->
            % Richiede se un parcheggio è libero ed aspetta il response.
            DetectPid ! {targetFree, false, Ref},
            receive
                {newParkingSpot, NewX, NewY} ->
                    % Use the new parking spot received from Detect.
                    state(Grid, {X, Y}, {NewX, NewY}, FriendshipPid)
            end
    end.

% Sospende l'esecuzione per N ms.
sleep(N) -> receive after N -> ok end.