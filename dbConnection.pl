:- module(dbConnection, [
    connect_to_database/0,
    disconnect_from_database/0,
    load_staff_data/0,
    load_available_slots/1,
    free_agenda_staff/3,
    staff/5,
    intersect_all_agendas/3
]).

:- use_module(library(odbc)).
:- dynamic staff/5. 
:- dynamic free_agenda_staff/3.

% Connect to the database
connect_to_database :-
    odbc_connect('hospitaldb', _Connection, [user('root'), password('K/C0QVM+rsI+'), alias(my_db), open(once)]).

% Disconnect from the database
disconnect_from_database :-
    odbc_disconnect(my_db).

% Load staff information from the Staff table
load_staff_data :-
    retractall(staff(_, _, _, _, _)),
    odbc_query(my_db, 'SELECT StaffId, LicenseNumber, Specialization, Email, Status FROM Staff',
               row(StaffId, LicenseNumber, Specialization, Email, Status)),
    assertz(staff(StaffId, LicenseNumber, Specialization, Email, Status)),
    fail.
load_staff_data.

% Load available slots for a specific date from AvailableSlots table
load_available_slots(Date) :-
    retractall(free_agenda_staff(_, _, _)),
    format(atom(Query), 'SELECT StaffId, StartTime, EndTime FROM AvailableSlots WHERE StartTime LIKE "~w%"', [Date]),
    findall((StaffId, StartMinutes, EndMinutes, DateNumber),
        (
            odbc_query(my_db, Query, row(StaffId, StartTime, EndTime)),
            time_to_minutes(StartTime, StartMinutes),
            time_to_minutes(EndTime, EndMinutes),
            StartTime = timestamp(Year, Month, Day, _, _, _, _),
            format(atom(DateStr), '~w~|~w~2+~|~w~2+', [Year, Month, Day]),
            atom_number(DateStr, DateNumber)
        ),
        SlotsData
    ),
    group_slots_by_staff_and_date(SlotsData).

% Group slots by staff and date
group_slots_by_staff_and_date(SlotsData) :-
    findall((StaffId, DateNumber),
        member((StaffId, _, _, DateNumber), SlotsData),
        UniquePairs),
    list_to_set(UniquePairs, UniqueStaffDates),
    forall(
        member((StaffId, DateNumber), UniqueStaffDates),
        (
            findall((StartMinutes, EndMinutes),
                member((StaffId, StartMinutes, EndMinutes, DateNumber), SlotsData),
                Slots),
            assertz(free_agenda_staff(StaffId, DateNumber, Slots))
        )
    ).

% Utility to convert SQL timestamp to minutes since midnight
time_to_minutes(timestamp(_, _, _, Hours, Minutes, _, _), TotalMinutes) :-
    TotalMinutes is Hours * 60 + Minutes.

% Intersect all agendas for a list of staff
intersect_all_agendas([], _, []) :- !.
intersect_all_agendas([StaffId], Date, FreeSlots) :- 
    normalize_date(Date, NormalizedDate),
    dbConnection:free_agenda_staff(StaffId, NormalizedDate, FreeSlots), !.
intersect_all_agendas([StaffId | Rest], Date, Intersection) :-
    normalize_date(Date, NormalizedDate),
    dbConnection:free_agenda_staff(StaffId, NormalizedDate, Slots1),
    intersect_all_agendas(Rest, NormalizedDate, Slots2),
    intersect_2_agendas(Slots1, Slots2, Intersection).

% Helper to normalize the date format
normalize_date(Date, Date) :- 
    integer(Date), !.  % If already normalized, return as-is
normalize_date(DateString, DateNumber) :-
    split_string(DateString, "-", "", [Year, Month, Day]),
    format(atom(DateAtom), "~w~|~w~2+~|~w~2+", [Year, Month, Day]),
    atom_number(DateAtom, DateNumber).

% Find the intersection between two sets of slots
intersect_2_agendas([], _, []).
intersect_2_agendas([Slot1 | Rest1], Slots2, Intersection) :-
    findall(
        (MaxStart, MinEnd),
        (member(Slot2, Slots2), intersect_slots(Slot1, Slot2, (MaxStart, MinEnd))),
        IntersectedSlots
    ),
    intersect_2_agendas(Rest1, Slots2, RestIntersection),
    append(IntersectedSlots, RestIntersection, Intersection).

% Calculate the intersection of two individual slots
intersect_slots((Start1, End1), (Start2, End2), (MaxStart, MinEnd)) :-
    MaxStart is max(Start1, Start2),
    MinEnd is min(End1, End2),
    MaxStart < MinEnd.
