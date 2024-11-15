% Staff.pl

:- dynamic availability/3.

% Clears existing availability and calculates free agendas for a given date
find_free_agendas(Date) :-
    retractall(availability(_, _, _)),  % Clear any previously stored availability
    % For each staff member with agenda on the given date
    forall(agenda_staff(StaffId, Date, OccupiedSlots), (
        % Calculate free intervals based on their agenda and timetable
        free_agenda_staff0(OccupiedSlots, FreeSlots),
        adapt_timetable(StaffId, Date, FreeSlots, AdjustedFreeSlots),
        % Assert the availability for this staff member on the given date
        assertz(availability(StaffId, Date, AdjustedFreeSlots))
    )).

% Calculate initial free intervals given occupied slots (from generic code)
free_agenda_staff0([], [(0, 1440)]).
free_agenda_staff0([(0, Tfin) | LT], LT1) :-
    !, free_agenda_staff1([(0, Tfin) | LT], LT1).
free_agenda_staff0([(Tin, Tfin) | LT], [(0, T1) | LT1]) :-
    T1 is Tin - 1,
    free_agenda_staff1([(Tin, Tfin) | LT], LT1).

free_agenda_staff1([(_, Tfin)], [(T1, 1440)]) :-
    Tfin \== 1440,
    !,
    T1 is Tfin + 1.
free_agenda_staff1([(_, _)], []).
free_agenda_staff1([(_, T), (T1, Tfin2) | LT], LT1) :-
    Tx is T + 1,
    T1 == Tx,
    !,
    free_agenda_staff1([(T1, Tfin2) | LT], LT1).
free_agenda_staff1([(_, Tfin1), (Tin2, Tfin2) | LT], [(T1, T2) | LT1]) :-
    T1 is Tfin1 + 1,
    T2 is Tin2 - 1,
    free_agenda_staff1([(Tin2, Tfin2) | LT], LT1).

% Adjust free intervals based on timetable (from generic code)
adapt_timetable(StaffId, Date, FreeSlots, AdjustedFreeSlots) :-
    timetable(StaffId, Date, (InTime, FinTime)),
    treatin(InTime, FreeSlots, TempFreeSlots),
    treatfin(FinTime, TempFreeSlots, AdjustedFreeSlots).

% Helper predicates to adjust free slots to fit within working hours (from generic code)
treatin(InTime, [(In, Fin) | FreeSlots], [(In, Fin) | FreeSlots]) :-
    InTime =< In, !.
treatin(InTime, [(_, Fin) | FreeSlots], AdjustedFreeSlots) :-
    InTime > Fin,
    !, treatin(InTime, FreeSlots, AdjustedFreeSlots).
treatin(InTime, [(_, Fin) | FreeSlots], [(InTime, Fin) | FreeSlots]).
treatin(_, [], []).

treatfin(FinTime, [(In, Fin) | FreeSlots], [(In, Fin) | AdjustedFreeSlots]) :-
    FinTime >= Fin,
    !, treatfin(FinTime, FreeSlots, AdjustedFreeSlots).
treatfin(FinTime, [(In, _) | _], []) :-
    FinTime =< In, !.
treatfin(FinTime, [(In, _) | _], [(In, FinTime)]).
treatfin(_, [], []).
