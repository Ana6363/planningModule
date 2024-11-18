:- module(dbConnection, [
    connect_to_database/0,
    disconnect_from_database/0
]).


% -------------------------------------------DATABASE CONNECTION AND DATA RETRIVAL---------------------------------------
:- use_module(library(odbc)).

% Connect to the database
connect_to_database :-
    odbc_connect('hospitaldb', _Connection, [user('root'), password('K/C0QVM+rsI+'), alias(my_db), open(once)]).

% Disconnect from the database
disconnect_from_database :-
    odbc_disconnect(my_db).

:- dynamic operation_data/6.
:- dynamic sorted_operation_request/6.
:- dynamic free_slot/4.

load_operation_data :-
    connect_to_database,
    retractall(operation_data(_, _, _, _, _, _)),
    odbc_query(my_db,
               '{CALL GetOperationData()}',
               row(OperationTypeId, OperationTypeName, PreparationTime, SurgeryTime, CleaningTime, Specializations)),
    split_specializations(Specializations, SpecializationList),
    assertz(operation_data(OperationTypeId, OperationTypeName, PreparationTime, SurgeryTime, CleaningTime, SpecializationList)),
    fail;
    disconnect_from_database.

split_specializations(SpecializationsString, SpecializationList) :-
    (   SpecializationsString == null
    ->  SpecializationList = []
    ;   split_string(SpecializationsString, ";", "", SpecializationPairs),
        maplist(parse_specialization, SpecializationPairs, SpecializationList)
    ).

parse_specialization(SpecializationPair, (Name, NeededPersonnel)) :-
    split_string(SpecializationPair, ":", "", [Name, NeededPersonnelString]),
    atom_number(NeededPersonnelString, NeededPersonnel).

load_and_sort_operation_requests :-
    connect_to_database,
    retractall(sorted_operation_request(_, _, _, _, _, _)),
    findall(row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status),
        odbc_query(my_db,
                   'SELECT RequestId, Deadline, Priority, RecordNumber, StaffId, Status FROM OperationRequests',
                   row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status)),
        Requests),
    sort_requests_by_priority_and_deadline(Requests, SortedRequests),
    maplist(assert_sorted_request, SortedRequests),
    disconnect_from_database.

assert_sorted_request(row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status)) :-
    format_deadline(Deadline, FormattedDeadline),
    assertz(sorted_operation_request(RequestId, FormattedDeadline, Priority, RecordNumber, StaffId, Status)).

sort_requests_by_priority_and_deadline(Requests, SortedRequests) :-
    get_today_date(Today),
    % Convert requests to sortable format
    maplist(convert_to_sortable(Today), Requests, SortableRequests),
    % Sort by priority and deadline
    keysort(SortableRequests, SortedSortableRequests),
    % Extract sorted requests
    maplist(extract_request, SortedSortableRequests, SortedRequests).

convert_to_sortable(Today, row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status),
                    SortKey-row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status)) :-
    priority_value(Priority, PriorityValue),
    format_deadline(Deadline, FormattedDeadline),
    days_difference(Today, FormattedDeadline, DaysUntilDeadline),
    SortKey = PriorityValue-DaysUntilDeadline.

extract_request(_-Request, Request).

priority_value('HIGH', 1).
priority_value('MEDIUM', 2).
priority_value('LOW', 3).

format_deadline(timestamp(Year, Month, Day, _, _, _, _), FormattedDeadline) :-
    format(atom(FormattedDeadlineAtom), '~d~|~`0t~d~2+~|~`0t~d~2+', [Year, Month, Day]),
    atom_number(FormattedDeadlineAtom, FormattedDeadline).

days_difference(Today, Deadline, Difference) :-
    Difference is Deadline - Today.

get_today_date(Today) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, 'UTC'),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    format(atom(TodayAtom), '~w~|~`0t~w~2+~|~`0t~w~2+', [Year, Month, Day]),
    atom_number(TodayAtom, Today).

fetch_occupied_slots(RoomId, Date, OccupiedSlots) :-
    connect_to_database,
    format(atom(Query), '{CALL GetOccupiedTimeSlots("~w", "~w")}', [RoomId, Date]),
    findall((StartMinute, EndMinute),
        odbc_query(my_db, Query, row(StartMinute, EndMinute)),
        OccupiedSlots),
    disconnect_from_database.

generate_and_save_free_slots(RoomId, Date, OccupiedSlots) :-
    find_free_intervals(480, 1200, OccupiedSlots, FreeIntervals),
    retractall(free_slot(RoomId, Date, _, _)),
    forall(
        member((FreeStart, FreeEnd), FreeIntervals),
        (
            assertz(free_slot(RoomId, Date, FreeStart, FreeEnd))
        )
    ).

find_free_intervals(StartOfDay, EndOfDay, [], [(StartOfDay, EndOfDay)]) :- !.
find_free_intervals(StartOfDay, EndOfDay, [(StartOccupied, EndOccupied) | Rest], FreeIntervals) :-
    StartOfDay < StartOccupied,
    NextStart is EndOccupied,
    find_free_intervals(NextStart, EndOfDay, Rest, RemainingIntervals),
    FreeIntervals = [(StartOfDay, StartOccupied) | RemainingIntervals].
find_free_intervals(StartOfDay, EndOfDay, [(StartOccupied, EndOccupied) | Rest], FreeIntervals) :-
    StartOfDay >= StartOccupied,
    NextStart is max(StartOfDay, EndOccupied),
    find_free_intervals(NextStart, EndOfDay, Rest, FreeIntervals).





