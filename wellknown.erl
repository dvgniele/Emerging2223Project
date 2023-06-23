-module(wellknown).
-export([wellknown/0]).

wellknown() ->
    register(wellknown, self()),
    io:format("##    Wellknown: actor registered as 'wellknown' with pid ~p    ##\n", [self()]),
    wellknown([]).

wellknown(PIDLIST) -> 
    %io:format("##    wellknown -> wellknown/1    ##\n"),
    receive
        {getFriends, PID1, PID2, Ref} ->
            % invio della lista di amici al processo di friendship che ha effettuato la richiesta
            PID1 ! {myFriends, PIDLIST, Ref},

            % controllo della presenza di {PID1, PID2} in PIDLIST
            % se non è contenuto, lo aggiunge
            case lists:member({PID1, PID2}, PIDLIST) of
                true -> wellknown(PIDLIST);
                false -> 
                    monitor(process, PID1),
                    io:format("##    Wellknown: adding new frenship for pid: ~p    ##\n", [PID1]),
                    % viene aggiunto successivamente alla restituzione della lista di friendship "myFriends"
                    % dato che non ha senso avere se stessi nella lista di amici
                    wellknown([{PID1, PID2} | PIDLIST])
            end
    end.