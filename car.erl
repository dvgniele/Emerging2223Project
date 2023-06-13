-module(car).
-export().

main() -> 
    Pid_frindship = spawn_link(?MODULE, friendship, []),
    Pid_state = spawn_link(?MODULE, state, []),
    Pid_detect = spawn_link(?MODULE, detect, []).

friendship(StatePid, FRIENDLIST) when FRIENDLIST =:= [] -> 
    link(StatePid),
    Ref = make_ref(),

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



state() -> .

detect() -> .