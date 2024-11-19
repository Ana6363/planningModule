:- module(dbConnection, [
    connect_to_database/0,
    disconnect_from_database/0
]).


% -------------------------------------------DATABASE CONNECTION AND DATA RETRIVAL---------------------------------------
:- use_module(library(odbc)).
:- use_module(library(lists)).

% Connect to the database
connect_to_database :-
    odbc_connect('hospitaldb', _Connection, [user('root'), password('K/C0QVM+rsI+'), alias(my_db), open(once)]).

% Disconnect from the database
disconnect_from_database :-
    odbc_disconnect(my_db).

:- dynamic operation_data/6.
:- dynamic operation_request/7.
:- dynamic free_slot/4.
:- dynamic staff/5.
:- dynamic scheduled_surgery/6.
:- dynamic stored_agenda/2.


initialize_environment :-
    retractall(scheduled_surgery(_, _, _, _)),
    retractall(free_slot(_,_,_,_)).
    
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

load_and_fetch_operation_requests(InputDate) :-
    connect_to_database,
    retractall(operation_request(_, _, _, _, _, _, _)),
    format(atom(Query),
           'SELECT RequestId, Deadline, Priority, RecordNumber, StaffId, Status, OperationType FROM OperationRequests WHERE Deadline > \'~w\' LIMIT 7',
           [InputDate]),
    findall(row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status, OperationType),
        odbc_query(my_db, Query, row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status, OperationType)),
        Requests),
    maplist(store_and_print_request, Requests),
    disconnect_from_database.

store_and_print_request(row(RequestId, timestamp(Year, Month, Day, _, _, _, _), Priority, RecordNumber, StaffId, Status, OperationType)) :-
    format_date_to_number(Year, Month, Day, FormattedDate),
    assertz(operation_request(RequestId, FormattedDate, Priority, RecordNumber, StaffId, Status, OperationType)),
    format('Stored fact: operation_request(~w, ~w, ~w, ~w, ~w, ~w, ~w)~n',
           [RequestId, FormattedDate, Priority, RecordNumber, StaffId, Status, OperationType]).



fetch_occupied_slots(RoomId, DateString, OccupiedSlots) :-
    connect_to_database,
    format_input_to_number(DateString, DateNumber), % Convert the date to a numeric format
    format(atom(Query), '{CALL GetOccupiedTimeSlots("~w", "~w")}', [RoomId, DateString]),
    findall((StartMinute, EndMinute),
        odbc_query(my_db, Query, row(StartMinute, EndMinute)),
        OccupiedSlots),
    format("DEBUG: Fetched occupied slots for RoomId: ~w, Date: ~w: ~w~n", [RoomId, DateNumber, OccupiedSlots]),
    disconnect_from_database.

generate_and_save_free_slots(RoomId, DateString, OccupiedSlots) :-
    retractall(free_slot(RoomId, DateNumber, _, _)),
    format_input_to_number(DateString, DateNumber), % Convert the date to a numeric format
    find_free_intervals(480, 1300, OccupiedSlots, FreeIntervals),
    retractall(free_slot(RoomId, DateNumber, _, _)), % Use numeric date format here
    forall(
        member((FreeStart, FreeEnd), FreeIntervals),
        (
            assertz(free_slot(RoomId, DateNumber, FreeStart, FreeEnd)) % Store numeric date
        )
    ),
    format("DEBUG: Saved free slots for RoomId: ~w, Date: ~w: ~w~n", [RoomId, DateNumber, FreeIntervals]),
    retractall(free_slot(main, _, _, _)).


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

fetch_staff_with_slots(Date) :-
    connect_to_database,
    retractall(staff(_, _, _, _, _)), % Clear existing staff facts
    format(atom(Query), '{CALL GetStaffWithSlots("~w")}', [Date]), % Construct the query
    findall(
        (StaffId, LicenseNumber, Specialization, Email, Slots),
        (
            odbc_query(my_db, Query, row(StaffId, LicenseNumber, Specialization, Email, StartMinutes, EndMinutes)),
            group_slots_by_staff(StaffId, StartMinutes, EndMinutes, Slots)
        ),
        StaffData
    ),
    disconnect_from_database,
    save_staff_facts(StaffData).

group_slots_by_staff(_, null, null, []) :- !. % No slots available
group_slots_by_staff(_, StartMinutes, EndMinutes, [(StartMinutes, EndMinutes)]) :-
    nonvar(StartMinutes), nonvar(EndMinutes).

save_staff_facts([]).
save_staff_facts([(StaffId, LicenseNumber, Specialization, Email, Slots) | Rest]) :-
    assertz(staff(StaffId, LicenseNumber, Specialization, Email, Slots)),
    save_staff_facts(Rest).

format_date_to_number(Year, Month, Day, FormattedDate) :-
    format(atom(DateAtom), '~d~|~`0t~d~2+~|~`0t~d~2+', [Year, Month, Day]),
    atom_number(DateAtom, FormattedDate).

format_input_to_number(DateInput, DateNumber) :-
    (atom(DateInput) -> atom_string(DateInput, DateString) ; DateString = DateInput),
    split_string(DateString, "-", "", [YearStr, MonthStr, DayStr]),
    atom_number(YearStr, Year),
    atom_number(MonthStr, Month),
    atom_number(DayStr, Day),
    DateNumber is Year * 10000 + Month * 100 + Day,
    format("DEBUG: DateNumber computed: ~w~n", [DateNumber]).    

% -------------------------------------------US 6.3.1---------------------------------------

schedule_all_surgeries(Room, Date, BestSchedule, EarliestFinishTime) :-
    write('DEBUG: Fetching all operation requests...'), nl,
    findall(Op, operation_request(_, _, _, _, _, _, Op), Operations),
    write('DEBUG: Operations fetched: '), write(Operations), nl,
    % Fetch room slots with proper debugging
    write('DEBUG: Fetching room slots for Room='), write(Room), write(', Date='), write(Date), nl,
    format_input_to_number(Date, DateNumber),
    findall((Start, End),
            dbConnection:free_slot(Room, DateNumber, Start, End),
            RoomSlots),
    write('DEBUG: Room slots fetched: '), write(RoomSlots), nl, % Removed second conflicting findall
    write('DEBUG: Generating all permutations of operations...'), nl,
    findall(Perm, permutation(Operations, Perm), Permutations),
    write('DEBUG: Generated permutations: '), length(Permutations, Len), write(Len), write(' permutations'), nl,
    find_best_schedule(Permutations, RoomSlots, Date, Room, BestSchedule, EarliestFinishTime),
    write('DEBUG: Best schedule: '), write(BestSchedule), nl,
    write('DEBUG: Earliest finish time: '), write(EarliestFinishTime), nl.

find_best_schedule([], _, _, _, [], 1300) :-
    write('DEBUG: No more permutations to evaluate. Initial best schedule is empty with finish time 1300.'), nl.
find_best_schedule([Perm | Perms], RoomSlots, Date, Room, BestSchedule, EarliestFinishTime) :-
    write('DEBUG: Evaluating permutation: '), write(Perm), nl,
    (evaluate_schedule(Perm, RoomSlots, Date, Room, Schedule, FinishTime) ->
        write('DEBUG: Valid schedule found for permutation: '), write(Schedule), nl,
        write('DEBUG: Finish time for this schedule: '), write(FinishTime), nl
    ;   write('DEBUG: Permutation failed to produce a valid schedule.'), nl, fail),
    find_best_schedule(Perms, RoomSlots, Date, Room, PrevBestSchedule, PrevEarliestFinishTime),
    write('DEBUG: Comparing schedules...'), nl,
    write('DEBUG: Current schedule: '), write(Schedule), write(' with finish time: '), write(FinishTime), nl,
    write('DEBUG: Previous best schedule: '), write(PrevBestSchedule), write(' with finish time: '), write(PrevEarliestFinishTime), nl,
    (FinishTime < PrevEarliestFinishTime ->
        write('DEBUG: Current schedule is better. Updating best schedule.'), nl,
        BestSchedule = Schedule,
        EarliestFinishTime = FinishTime
    ;   write('DEBUG: Previous best schedule remains the best.'), nl,
        BestSchedule = PrevBestSchedule,
        EarliestFinishTime = PrevEarliestFinishTime).


evaluate_schedule([], _, _, _, [], 0) :-
    write('DEBUG: Empty schedule. Finish time is 0.'), nl.
evaluate_schedule([Op | Ops], RoomSlots, Date, Room, [(Op, StartTime, EndTime) | RestSchedule], FinishTime) :-
    write('DEBUG: Evaluating operation: '), write(Op), nl,
    operation_total_duration(Op, Duration),
    write('DEBUG: Total duration for operation '), write(Op), write(': '), write(Duration), nl,
    operation_data(_, Op, _, _, _, StaffRequirements),
    write('DEBUG: Staff requirements for operation '), write(Op), write(': '), write(StaffRequirements), nl,
    % Find a valid slot without modifying RoomSlots prematurely
    (intersect_room_staff(RoomSlots, Date, Duration, StaffRequirements, StartTime, EndTime) ->
        write('DEBUG: Found valid slot for operation '), write(Op), 
        write(': Start='), write(StartTime), write(', End='), write(EndTime), nl,
        % Confirm and remove the assigned time slot
        remove_time_slot(RoomSlots, (StartTime, EndTime), UpdatedRoomSlots),
        % Continue scheduling remaining operations
        evaluate_schedule(Ops, UpdatedRoomSlots, Date, Room, RestSchedule, RestFinishTime),
        FinishTime is max(EndTime, RestFinishTime)
    ;   % Fail if no valid slot is found
        write('DEBUG: No valid slot found for operation '), write(Op), nl, fail).





intersect_room_staff(RoomSlots, Date, Duration, StaffRequirements, StartTime, EndTime) :-
    write('DEBUG: Intersecting all staff agendas for requirements: '), write(StaffRequirements), nl,
    intersect_all_staff_agendas(StaffRequirements, Date, StaffSlots),
    write('DEBUG: Combined staff slots: '), write(StaffSlots), nl,
    % Intersect RoomSlots with combined StaffSlots
    write(RoomSlots),
    intersect_2_agendas(RoomSlots, StaffSlots, ValidSlots),
    write('DEBUG: Valid slots after intersection with room slots: '), write(ValidSlots), nl,
    remove_unf_intervals(Duration, ValidSlots, [(StartTime, EndTime) | _]),
    write('DEBUG: Selected slot: Start='), write(StartTime), write(', End='), write(EndTime), nl.


intersect_all_staff_agendas([], _, []). % Base case: no roles left, empty combined agenda.
intersect_all_staff_agendas(StaffRequirements, _, CombinedAgenda) :-
    % Collect agendas for all roles
    findall(RoleAgenda,
            (
                member((Role, _), StaffRequirements), % Iterate through each role
                write('DEBUG: Calculating agenda for role: '), write(Role), nl,
                normalize_role(Role, NormalizedRole), % Normalize role name
                findall((Start, End),
                        (
                            staff(_, _, Specialization, _, Slots),
                            normalize_role(Specialization, NormalizedSpec),
                            sub_atom(NormalizedSpec, 0, _, 0, NormalizedRole),
                            member((Start, End), Slots)
                        ),
                        RoleAgendaRaw),
                deduplicate_slots(RoleAgendaRaw, RoleAgenda),
                write('DEBUG: Calculated agenda for role '), write(NormalizedRole), write(': '), write(RoleAgenda), nl
            ),
            AllAgendas),

    intersect_all_agendas(AllAgendas, CombinedAgenda),
    write('DEBUG: Combined staff slots: '), write(CombinedAgenda), nl.


intersect_all_agendas([], []). % If no agendas, the result is empty.
intersect_all_agendas([Agenda], Agenda). % A single agenda remains unchanged.
intersect_all_agendas([Agenda1, Agenda2 | Rest], CombinedAgenda) :-
    % Intersect the first two agendas
    intersect_2_agendas(Agenda1, Agenda2, TempCombined),
    % Recursively intersect with the rest of the agendas
    intersect_all_agendas([TempCombined | Rest], CombinedAgenda).

% Intersection of two agendas
intersect_2_agendas([], _, []) :- !. % Empty first agenda results in no intersections.
intersect_2_agendas(_, [], []) :- !. % Empty second agenda results in no intersections.
intersect_2_agendas([(Start1, End1) | Rest1], [(Start2, End2) | Rest2], Result) :-
    MaxStart is max(Start1, Start2),
    MinEnd is min(End1, End2),
    (MaxStart < MinEnd ->
        % Valid overlap; include it in the result
        Result = [(MaxStart, MinEnd) | RestResult],
        intersect_2_agendas(Rest1, [(Start2, End2) | Rest2], RestResult)
    ;   % No overlap or degenerate interval
        (End1 =< Start2 ->
            intersect_2_agendas(Rest1, [(Start2, End2) | Rest2], Result)
        ;   intersect_2_agendas([(Start1, End1) | Rest1], Rest2, Result)
        )
    ).
      

remove_unf_intervals(_, [], []).
remove_unf_intervals(Duration, [(Start, End) | Rest], [(Start, End) | ValidRest]) :-
    write('DEBUG: Checking interval: Start='), write(Start), write(', End='), write(End), write(', Duration='), write(Duration), nl,
    End - Start >= Duration,
    remove_unf_intervals(Duration, Rest, ValidRest).
remove_unf_intervals(Duration, [_ | Rest], ValidRest) :-
    remove_unf_intervals(Duration, Rest, ValidRest).

% Remove a used time slot from a list of slots
remove_time_slot([], _, []).
remove_time_slot([(Start, End) | Rest], (UsedStart, UsedEnd), UpdatedSlots) :-
    (UsedEnd =< Start ; UsedStart >= End), % No overlap
    remove_time_slot(Rest, (UsedStart, UsedEnd), UpdatedRest),
    UpdatedSlots = [(Start, End) | UpdatedRest].
remove_time_slot([(Start, End) | Rest], (UsedStart, UsedEnd), UpdatedSlots) :-
    (UsedStart > Start -> UpdatedSlots = [(Start, UsedStart) | PartialUpdatedSlots] ; PartialUpdatedSlots = UpdatedSlots),
    (UsedEnd < End -> PartialUpdatedSlots = [(UsedEnd, End) | UpdatedRest] ; UpdatedRest = []),
    remove_time_slot(Rest, (UsedStart, UsedEnd), UpdatedRest).

% Define operation_total_duration/2
operation_total_duration(Operation, TotalDuration) :-
    operation_data(_, Operation, PreparationTime, OperationTime, CleaningTime, _),
    TotalDuration is PreparationTime + OperationTime + CleaningTime,
    write(TotalDuration).

normalize_role(Role, NormalizedRole) :-
    atom_codes(Role, Codes),
    maplist(to_lower, Codes, LowerCodes),
    exclude(=(32), LowerCodes, CleanCodes), % Remove spaces (ASCII 32)
    atom_codes(NormalizedRole, CleanCodes).

deduplicate_slots(Slots, DeduplicatedSlots) :-
    sort(Slots, DeduplicatedSlots). % Removes duplicates and sorts intervals.
