:- module(dbConnection, [
    connect_to_database/0,
    disconnect_from_database/0,
    load_staff_data/0,
    load_available_slots/1,
    staff/5,  % Add staff/5 to the export list
    free_agenda_staff/3
]).


:- use_module(library(odbc)).
:- dynamic staff/5. 
:- dynamic agenda_staff/3.

connect_to_database :-
    odbc_connect('hospitaldb', _Connection, [user('root'), password('K/C0QVM+rsI+'), alias(my_db), open(once)]).

disconnect_from_database :-
    odbc_disconnect(my_db).

load_staff_data :-
    retractall(staff(_, _, _, _, _)),
    odbc_query(my_db, 'SELECT StaffId, LicenseNumber, Specialization, Email, Status FROM Staff',
               row(StaffId, LicenseNumber, Specialization, Email, Status)),
    assertz(staff(StaffId, LicenseNumber, Specialization, Email, Status)),
    fail.
load_staff_data.

load_available_slots(Date) :-
    retractall(free_agenda_staff(_, _, _)),  % Clear previous data
    format(atom(Query), 'SELECT StaffId, StartTime, EndTime FROM AvailableSlots WHERE StartTime LIKE "~w%"', [Date]),
    format("Executing Query: ~w~n", [Query]),  % Debug: Print the SQL query
    findall((StaffId, StartMinutes, EndMinutes, DateNumber),
        (
            odbc_query(my_db, Query, row(StaffId, StartTime, EndTime)),
            % Convert StartTime and EndTime to time intervals
            time_to_minutes(StartTime, StartMinutes),
            time_to_minutes(EndTime, EndMinutes),
            % Extract date from StartTime
            StartTime = timestamp(Year, Month, Day, _, _, _, _),
            format(atom(DateStr), '~w~|~w~2+~|~w~2+', [Year, Month, Day]),
            atom_number(DateStr, DateNumber)
        ),
        SlotsData
    ),
    group_slots_by_staff_and_date(SlotsData).

group_slots_by_staff_and_date(SlotsData) :-
    % Collect all unique (StaffId, DateNumber) pairs
    findall((StaffId, DateNumber),
        member((StaffId, _, _, DateNumber), SlotsData),
        UniquePairs),
    list_to_set(UniquePairs, UniqueStaffDates),
    % For each unique pair, collect the slots
    forall(
        member((StaffId, DateNumber), UniqueStaffDates),
        (
            findall((StartMinutes, EndMinutes),
                member((StaffId, StartMinutes, EndMinutes, DateNumber), SlotsData),
                Slots),
            % Assert the aggregated slots for the staff and date
            assertz(free_agenda_staff(StaffId, DateNumber, Slots))
        )
    ).

% Utility to convert SQL timestamp to minutes since midnight
time_to_minutes(timestamp(_, _, _, Hours, Minutes, _, _), TotalMinutes) :-
    TotalMinutes is Hours * 60 + Minutes.

