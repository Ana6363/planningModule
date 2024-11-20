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
       'SELECT RequestId, Deadline, Priority, RecordNumber, StaffId, Status, OperationType FROM OperationRequests WHERE Deadline > \'~w\' AND Status = \'PENDING\' LIMIT 4',
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
    findall((OpId, OpName), operation_request(OpId, _, _, _, _, _, OpName), Operations),
    write('DEBUG: Operations fetched: '), write(Operations), nl,
    length(Operations, NumOperations),
    write('DEBUG: Total number of operations fetched: '), write(NumOperations), nl,
    write('DEBUG: Fetching room slots for Room='), write(Room), write(', Date='), nl,
    format_input_to_number(Date, DateNumber),
    findall((Start, End),
            dbConnection:free_slot(Room, DateNumber, Start, End),
            RoomSlots),
    write('DEBUG: Room slots fetched: '), write(RoomSlots), nl,
    write('DEBUG: Generating all possible permutations...'), nl,
    findall(Permutation, permutation(Operations, Permutation), Permutations),
    write('DEBUG: Generated permutations:'), nl,
    write(Permutations), nl,
    write('DEBUG: Total permutations generated: '), length(Permutations, PermutationCount), write(PermutationCount), nl,
    find_best_schedule(Permutations, RoomSlots, Date, Room, BestSchedule, EarliestFinishTime),
    preprocess_schedule(BestSchedule, PreprocessedSchedule),
    filter_best_schedule(PreprocessedSchedule, ValidBestSchedule),
    display_schedule_and_confirm(ValidBestSchedule, Date, Room),
    write('DEBUG: Earliest finish time: '), write(EarliestFinishTime), nl.


custom_subset([], []).
custom_subset([H | T], [H | SubT]) :- custom_subset(T, SubT).
custom_subset([_ | T], SubT) :- custom_subset(T, SubT).

print_subsets([]).
print_subsets([H]) :- 
    write('    '), write(H), nl. % For the last subset (no trailing comma)
print_subsets([H | T]) :- 
    write('    '), write(H), write(','), nl, 
    print_subsets(T).

find_best_schedule([], _, _, _, [], 1300) :-
    write('DEBUG: No more combinations to evaluate. Initial best schedule is empty with finish time 1300.'), nl.

find_best_schedule([Combination | Combinations], RoomSlots, Date, Room, BestSchedule, EarliestFinishTime) :-
    write('DEBUG: Evaluating combination: '), write(Combination), nl,
    (evaluate_schedule(Combination, RoomSlots, Date, Room, Schedule, FinishTime) ->
        write('DEBUG: Valid schedule found: '), write(Schedule), nl,
        write('DEBUG: Finish time: '), write(FinishTime), nl
    ;   write('DEBUG: Combination failed to produce a valid schedule.'), nl,
        Schedule = [], FinishTime = 1300), % Default invalid values

    find_best_schedule(Combinations, RoomSlots, Date, Room, PrevBestSchedule, PrevEarliestFinishTime),    
    length(Schedule, Len1),
    length(PrevBestSchedule, Len2),
    compare_schedules(Schedule, Len1, FinishTime, PrevBestSchedule, Len2, PrevEarliestFinishTime, BestSchedule, EarliestFinishTime).

compare_schedules(Schedule, Len1, FinishTime, PrevBestSchedule, Len2, PrevEarliestFinishTime, BestSchedule, EarliestFinishTime) :-
    (Len1 > Len2 ->
        write('DEBUG: Current schedule fits more operations. Updating best schedule.'), nl,
        BestSchedule = Schedule,
        EarliestFinishTime = FinishTime
    ; Len1 == Len2, FinishTime < PrevEarliestFinishTime ->
        write('DEBUG: Current schedule finishes earlier. Updating best schedule.'), nl,
        BestSchedule = Schedule,
        EarliestFinishTime = FinishTime
    ;   write('DEBUG: Previous best schedule remains the best.'), nl,
        BestSchedule = PrevBestSchedule,
        EarliestFinishTime = PrevEarliestFinishTime).

evaluate_schedule([], _, _, _, [], 0) :-
    write('DEBUG: Empty schedule. Finish time is 0.'), nl.

evaluate_schedule([(RequestId, OpName) | Ops], RoomSlots, Date, Room, [(RequestId, OpName, StartTime, EndTime) | RestSchedule], FinishTime) :-
    write('DEBUG: Evaluating operation: '), write(OpName), nl,
    operation_total_duration(OpName, Duration), % Use only the operation name for duration
    write('DEBUG: Total duration for operation '), write(OpName), write(': '), write(Duration), nl,
    operation_data(_, OpName, _, _, _, StaffRequirements), % Use only the operation name for staff requirements
    write('DEBUG: Staff requirements for operation '), write(OpName), write(': '), write(StaffRequirements), nl,
    (intersect_room_staff(RoomSlots, Date, Duration, StaffRequirements, StartTime, EndTime) ->
        write('DEBUG: Found valid slot: Start='), write(StartTime), write(', End='), write(EndTime), nl,
        remove_time_slot(RoomSlots, (StartTime, EndTime), UpdatedRoomSlots),
        evaluate_schedule(Ops, UpdatedRoomSlots, Date, Room, RestSchedule, RestFinishTime),
        FinishTime is max(EndTime, RestFinishTime)
    ;   write('DEBUG: No valid slot found for operation '), write(OpName), nl,
        evaluate_schedule(Ops, RoomSlots, Date, Room, RestSchedule, FinishTime)).

intersect_room_staff(RoomSlots, Date, Duration, StaffRequirements, StartTime, EndTime) :-
    write('DEBUG: Intersecting all staff agendas for requirements: '), write(StaffRequirements), nl,
    intersect_all_staff_agendas(StaffRequirements, Date, StaffSlots),
    write('DEBUG: Combined staff slots: '), write(StaffSlots), nl,
    write(RoomSlots),
    intersect_2_agendas(RoomSlots, StaffSlots, ValidSlots),
    write('DEBUG: Valid slots after intersection with room slots: '), write(ValidSlots), nl,
    remove_unf_intervals(Duration, ValidSlots, [(StartTime, EndTime) | _]),
    write('DEBUG: Selected slot: Start='), write(StartTime), write(', End='), write(EndTime), nl.

intersect_all_staff_agendas([], _, []).

intersect_all_staff_agendas(StaffRequirements, _, CombinedAgenda) :-
    findall(RoleAgenda,
            (
                % Iterate through each role in the staff requirements
                member((Role, _), StaffRequirements),
                write('DEBUG: Calculating agenda for role: '), write(Role), nl,
                normalize_role(Role, NormalizedRole),
                findall((Start, End),
                        (
                            staff(_, _, Specialization, _, Slots), % Access staff slots
                            normalize_role(Specialization, NormalizedSpec), % Normalize specialization name
                            sub_atom(NormalizedSpec, 0, _, 0, NormalizedRole), % Match role with specialization
                            member((Start, End), Slots) % Collect matching slots
                        ),
                        RoleAgendaRaw),
                write('DEBUG: Raw slots for role '), write(NormalizedRole), write(': '), write(RoleAgendaRaw), nl,
                merge_slots(RoleAgendaRaw, RoleAgenda),
                write('DEBUG: Calculated agenda for role '), write(NormalizedRole), write(': '), write(RoleAgenda), nl
            ),
            AllAgendas),

    write('DEBUG: Agendas being intersected for all roles: '), write(AllAgendas), nl,
    intersect_all_agendas(AllAgendas, CombinedAgenda),
    write('DEBUG: Combined staff slots: '), write(CombinedAgenda), nl.

intersect_all_agendas([], []). % If no agendas, the result is empty.
intersect_all_agendas([Agenda], Agenda). % A single agenda remains unchanged.
intersect_all_agendas([Agenda1, Agenda2 | Rest], CombinedAgenda) :-
    % Intersect the first two agendas
    intersect_2_agendas(Agenda1, Agenda2, TempCombined),
    % Recursively intersect with the rest of the agendas
    intersect_all_agendas([TempCombined | Rest], CombinedAgenda).

intersect_2_agendas([], _, []) :- !. % Empty first agenda results in no intersections.
intersect_2_agendas(_, [], []) :- !. % Empty second agenda results in no intersections.

intersect_2_agendas([(Start1, End1) | Rest1], [(Start2, End2) | Rest2], Result) :-
    MaxStart is max(Start1, Start2),
    MinEnd is min(End1, End2),
    (MaxStart < MinEnd ->
        Result = [(MaxStart, MinEnd) | RestResult],
        % Move to the next interval, advancing based on the earliest end
        (End1 =< End2 ->
            intersect_2_agendas(Rest1, [(Start2, End2) | Rest2], RestResult)
        ;   intersect_2_agendas([(Start1, End1) | Rest1], Rest2, RestResult)
        )
    ;
        (End1 =< Start2 ->
            intersect_2_agendas(Rest1, [(Start2, End2) | Rest2], Result)
        ;   intersect_2_agendas([(Start1, End1) | Rest1], Rest2, Result)
        )
    ).

remove_unf_intervals(_, [], []).

remove_unf_intervals(Duration, [(Start, End) | Rest], [(Start, AdjustedEnd) | ValidRest]) :-
    End - Start >= Duration,
    AdjustedEnd is Start + Duration,
    remove_unf_intervals(Duration, Rest, ValidRest).

remove_unf_intervals(Duration, [_ | Rest], ValidRest) :-
    remove_unf_intervals(Duration, Rest, ValidRest).

remove_time_slot([], _, []).
remove_time_slot([(Start, End) | Rest], (UsedStart, UsedEnd), UpdatedSlots) :-
    (UsedEnd =< Start ; UsedStart >= End), % No overlap
    remove_time_slot(Rest, (UsedStart, UsedEnd), UpdatedRest),
    UpdatedSlots = [(Start, End) | UpdatedRest].
remove_time_slot([(Start, End) | Rest], (UsedStart, UsedEnd), UpdatedSlots) :-
    (UsedStart > Start -> UpdatedSlots = [(Start, UsedStart) | PartialUpdatedSlots] ; PartialUpdatedSlots = UpdatedSlots),
    (UsedEnd < End -> PartialUpdatedSlots = [(UsedEnd, End) | UpdatedRest] ; UpdatedRest = []),
    remove_time_slot(Rest, (UsedStart, UsedEnd), UpdatedRest).

operation_total_duration(Operation, TotalDuration) :-
    operation_data(_, Operation, PreparationTime, OperationTime, CleaningTime, _),
    TotalDuration is PreparationTime + OperationTime + CleaningTime,
    write(TotalDuration).

normalize_role(Role, NormalizedRole) :-
    atom_codes(Role, Codes),
    maplist(to_lower, Codes, LowerCodes),
    exclude(=(32), LowerCodes, CleanCodes), % Remove spaces (ASCII 32)
    atom_codes(NormalizedRole, CleanCodes).

merge_slots(RawSlots, MergedSlots) :-
    % Sort the raw slots by their start time
    sort(1, @=<, RawSlots, SortedSlots),
    % Merge overlapping or adjacent slots
    merge_sorted_slots(SortedSlots, MergedSlots).

merge_sorted_slots([Slot], [Slot]) :- !.

merge_sorted_slots([(Start1, End1), (Start2, End2) | Rest], MergedSlots) :-
    (End1 >= Start2 -> 
        % Overlapping or adjacent intervals, merge them
        NewEnd is max(End1, End2),
        merge_sorted_slots([(Start1, NewEnd) | Rest], MergedSlots)
    ;   
        % No overlap, keep the first interval and continue
        merge_sorted_slots([(Start2, End2) | Rest], RestMerged),
        MergedSlots = [(Start1, End1) | RestMerged]
    ).

filter_best_schedule([], []).

filter_best_schedule([(RequestId, OperationName, StartTime, EndTime) | Rest], [(RequestId, OperationName, StartTime, EndTime) | FilteredRest]) :-
    number(StartTime), % Ensure StartTime is a valid number
    number(EndTime),   % Ensure EndTime is a valid number
    filter_best_schedule(Rest, FilteredRest).

filter_best_schedule([(RequestId, OperationName, StartTime, EndTime) | Rest], FilteredRest) :-
    \+ number(StartTime), % Exclude if StartTime is not a number
    \+ number(EndTime),   % Exclude if EndTime is not a number
    write('DEBUG: Excluded request with invalid times: '), write((RequestId, OperationName, StartTime, EndTime)), nl,
    filter_best_schedule(Rest, FilteredRest).

preprocess_schedule([], []).

preprocess_schedule([(Id, OperationName, StartTime, EndTime) | Rest], [(QuotedId, QuotedOperationName, StartTime, EndTime) | ProcessedRest]) :-
    atom(Id), atom(OperationName), % Ensure Id and OperationName are atoms
    quote_if_needed(Id, QuotedId),
    quote_if_needed(OperationName, QuotedOperationName),
    preprocess_schedule(Rest, ProcessedRest).

quote_if_needed(Atom, Atom) :- atom_chars(Atom, Chars), \+ member('-', Chars), !.
quote_if_needed(Atom, QuotedAtom) :- atom(Atom), !, atom_string(Atom, String), atom_string(QuotedAtom, String).

% ----------------------------------------------UTILITIES-----------------------------------------------------------------
display_schedule_and_confirm(BestSchedule, Date, Room) :-
    write('Final Surgery Schedule for Room '), write(Room), write(' on '), write(Date), nl,
    display_schedule(BestSchedule),
    write('Do you want to apply this schedule? (yes/no)'), nl,
    read(Confirmation),
    (Confirmation == yes ->
        apply_schedule(BestSchedule, Date, Room)
    ;   write('Schedule application canceled.'), nl).

display_schedule([]).
display_schedule([(RequestId, OperationName, StartTime, EndTime) | Rest]) :-
    write('Request ID: '), write(RequestId), nl,
    write('Operation: '), write(OperationName), nl,
    write('Start Time: '), format_time(StartTime), nl,
    write('End Time: '), format_time(EndTime), nl, nl,
    display_schedule(Rest).

format_time(Minutes) :-
    Hours is Minutes // 60,
    Mins is Minutes mod 60,
    format('~|~`0t~d~2+:~|~`0t~d~2+', [Hours, Mins]).

apply_schedule(BestSchedule, Date, Room) :-
    connect_to_database,
    update_requests_status(BestSchedule),
    create_appointments_and_phases(BestSchedule, Date, Room),
    write('Schedule successfully applied!'), nl.

update_requests_status([]).
update_requests_status([(RequestId, _, _, _) | Rest]) :-
    format(atom(Query), 'UPDATE OperationRequests SET Status = "ACCEPTED" WHERE RequestId = "~w"', [RequestId]),
    db_execute(Query), % Use the updated db_execute method
    update_requests_status(Rest).


create_appointments_and_phases([], _, _) :-
    write('DEBUG: Finished creating appointments and phases.'), nl.
create_appointments_and_phases([(RequestId, OperationName, StartTime, EndTime) | Rest], Date, Room) :-
    write('DEBUG: Creating appointment for RequestId='), write(RequestId), nl,
    create_appointment(RequestId, OperationName, StartTime, EndTime, Date, AppointmentId),
    write('DEBUG: Appointment created with ID='), write(AppointmentId), nl,
    write('DEBUG: Inserting phases for AppointmentId='), write(AppointmentId), nl,
    insert_phases(OperationName, Room, AppointmentId, StartTime, Date),
    create_appointments_and_phases(Rest, Date, Room).


create_appointment(RequestId, OperationName, StartTime, EndTime, Date, AppointmentId) :-
    generate_random_string(AppointmentId),
    format_time(StartTime, StartFormatted), % Convert StartTime to HH:MM format
    format(atom(Schedule), '~w ~w:00', [Date, StartFormatted]), % Only use the start time for the schedule
    db_fetch('OperationRequests', RequestId, [Patient, Staff]), % Fetch Patient and Staff details
    format(atom(Query), 
        'INSERT INTO Appointements (AppointementId, Schedule, Request, Patient, Staff) VALUES ("~w", "~w", "~w", "~w", "~w")', 
        [AppointmentId, Schedule, RequestId, Patient, Staff]), % Insert only the start time as schedule
    db_execute(Query).



insert_phases(OperationName, Room, AppointmentId, StartTime, Date) :-
    write('DEBUG: Fetching operation data for RequestId='), write(OperationName), nl,
    operation_data(_, OperationName, PrepTime, SurgeryTime, CleaningTime, _),
    write('DEBUG: Preparation Time='), write(PrepTime), write(', Surgery Time='), write(SurgeryTime), write(', Cleaning Time='), write(CleaningTime), nl,
    insert_phase(AppointmentId, Room, 'Preparation', PrepTime, StartTime, Date, PrepEndTime),
    insert_phase(AppointmentId, Room, 'Surgery', SurgeryTime, PrepEndTime, Date, SurgeryEndTime),
    insert_phase(AppointmentId, Room, 'Cleaning', CleaningTime, SurgeryEndTime, Date, _).


insert_phase(AppointmentId, Room, PhaseType, Duration, StartTime, Date, EndTime) :-
    generate_random_number(PhaseId),
    EndTime is StartTime + Duration, 
    format_time(StartTime, StartFormatted), 
    format_time(EndTime, EndFormatted),    
    format(atom(StartDatetime), '~w ~w:00', [Date, StartFormatted]), 
    format(atom(EndDatetime), '~w ~w:00', [Date, EndFormatted]),
    format(atom(Query), 
        'INSERT INTO SurgeryPhaseDataModel (Id, RoomNumber, PhaseType, Duration, StartTime, EndTime, AppointementId) VALUES ("~w", "~w", "~w", "~w", "~w", "~w", "~w")', 
        [PhaseId, Room, PhaseType, Duration, StartDatetime, EndDatetime, AppointmentId]),
    write('Executing Phase Insert Query: '), write(Query), nl,
    db_execute(Query).

db_execute(Query) :-
    write('Executing Query: '), write(Query), nl, % Debugging output
    odbc_query(my_db, Query, _).

db_fetch('OperationRequests', RequestId, [RecordNumber, StaffId]) :-
    format(atom(Query), 'SELECT RecordNumber, StaffId FROM OperationRequests WHERE RequestId = "~w"', [RequestId]),
    write('Executing Query: '), write(Query), nl, % Debugging output
    odbc_query(my_db, Query, row(RecordNumber, StaffId)).

generate_random_string(RandomString) :-
    uuid(RandomString).

generate_random_number(RandomNumber) :-
    random_between(100000, 999999, RandomNumber). % Generate a 6-digit random number

format_time(Minutes, FormattedTime) :-
    Hours is Minutes // 60,
    Mins is Minutes mod 60,
    format(atom(FormattedTime), '~|~`0t~d~2+:~|~`0t~d~2+', [Hours, Mins]).

