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

load_and_fetch_operation_requests(InputDate, Limit) :-
    connect_to_database,
    retractall(operation_request(_, _, _, _, _, _, _)),
    format(atom(Query),
           'SELECT RequestId, Deadline, Priority, RecordNumber, StaffId, Status, OperationType FROM OperationRequests WHERE Deadline > \'~w\' AND Status = \'PENDING\' LIMIT ~w',
           [InputDate, Limit]),
    findall(row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status, OperationType),
            odbc_query(my_db, Query, row(RequestId, Deadline, Priority, RecordNumber, StaffId, Status, OperationType)),
            Requests),
    maplist(store_and_print_request, Requests), % Use the updated store_request predicate without printing
    disconnect_from_database.


store_and_print_request(row(RequestId, timestamp(Year, Month, Day, _, _, _, _), Priority, RecordNumber, StaffId, Status, OperationType)) :-
    format_date_to_number(Year, Month, Day, FormattedDate),
    assertz(operation_request(RequestId, FormattedDate, Priority, RecordNumber, StaffId, Status, OperationType)).




fetch_occupied_slots(RoomId, DateString, OccupiedSlots) :-
    connect_to_database,
    format(atom(Query), '{CALL GetOccupiedTimeSlots("~w", "~w")}', [RoomId, DateString]),
    findall((StartMinute, EndMinute),
        odbc_query(my_db, Query, row(StartMinute, EndMinute)),
        OccupiedSlots),
    disconnect_from_database.

generate_and_save_free_slots(RoomId, DateString, OccupiedSlots) :-
    retractall(free_slot(RoomId, DateNumber, _, _)),
    format_input_to_number(DateString, DateNumber),
    find_free_intervals(480, 1300, OccupiedSlots, FreeIntervals),
    retractall(free_slot(RoomId, DateNumber, _, _)),
    forall(
        member((FreeStart, FreeEnd), FreeIntervals),
        (
            assertz(free_slot(RoomId, DateNumber, FreeStart, FreeEnd))
        )
    ),
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
    retractall(staff(_, _, _, _, _)),
    format(atom(Query), '{CALL GetStaffWithSlots("~w")}', [Date]),
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

group_slots_by_staff(_, null, null, []) :- !.
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
    DateNumber is Year * 10000 + Month * 100 + Day.

% -------------------------------------------US 6.3.1---------------------------------------

schedule_all_surgeries(Room, Date, BestSchedule, EarliestFinishTime) :-
    findall((OpId, OpName), operation_request(OpId, _, _, _, _, _, OpName), Operations),
    length(Operations, _),
    format_input_to_number(Date, DateNumber),
    findall((Start, End),
            dbConnection:free_slot(Room, DateNumber, Start, End),
            RoomSlots),
    findall(Permutation, permutation(Operations, Permutation), Permutations),
    find_best_schedule(Permutations, RoomSlots, Date, Room, BestSchedule, EarliestFinishTime),
    preprocess_schedule(BestSchedule, PreprocessedSchedule),
    filter_best_schedule(PreprocessedSchedule, ValidBestSchedule),
    assign_staff_to_schedule(ValidBestSchedule, FinalSchedule),
    display_schedule_and_confirm(FinalSchedule, Date, Room).


custom_subset([], []).
custom_subset([H | T], [H | SubT]) :- custom_subset(T, SubT).
custom_subset([_ | T], SubT) :- custom_subset(T, SubT).

find_best_schedule([], _, _, _, [], 1300).

find_best_schedule([Combination | Combinations], RoomSlots, Date, Room, BestSchedule, EarliestFinishTime) :-
    (evaluate_schedule(Combination, RoomSlots, Date, Room, Schedule, FinishTime) ->
        true
    ;   Schedule = [], FinishTime = 1300),
    find_best_schedule(Combinations, RoomSlots, Date, Room, PrevBestSchedule, PrevEarliestFinishTime),    
    length(Schedule, Len1),
    length(PrevBestSchedule, Len2),
    compare_schedules(Schedule, Len1, FinishTime, PrevBestSchedule, Len2, PrevEarliestFinishTime, BestSchedule, EarliestFinishTime).


compare_schedules(Schedule, Len1, FinishTime, PrevBestSchedule, Len2, PrevEarliestFinishTime, BestSchedule, EarliestFinishTime) :-
    (Len1 > Len2 ->
        BestSchedule = Schedule,
        EarliestFinishTime = FinishTime
    ; Len1 == Len2, FinishTime < PrevEarliestFinishTime ->
        BestSchedule = Schedule,
        EarliestFinishTime = FinishTime
    ;
        BestSchedule = PrevBestSchedule,
        EarliestFinishTime = PrevEarliestFinishTime).

evaluate_schedule([], _, _, _, [], 0).

evaluate_schedule([(RequestId, OpName) | Ops], RoomSlots, Date, Room, [(RequestId, OpName, StartTime, EndTime) | RestSchedule], FinishTime) :-
    operation_total_duration(OpName, Duration),
    operation_data(_, OpName, _, _, _, StaffRequirements),
    (intersect_room_staff(RoomSlots, Date, Duration, StaffRequirements, StartTime, EndTime) ->
        remove_time_slot(RoomSlots, (StartTime, EndTime), UpdatedRoomSlots),
        evaluate_schedule(Ops, UpdatedRoomSlots, Date, Room, RestSchedule, RestFinishTime),
        FinishTime is max(EndTime, RestFinishTime)
    ;
        evaluate_schedule(Ops, RoomSlots, Date, Room, RestSchedule, FinishTime)).

intersect_room_staff(RoomSlots, Date, Duration, StaffRequirements, StartTime, EndTime) :-
    intersect_all_staff_agendas(StaffRequirements, Date, StaffSlots),
    intersect_2_agendas(RoomSlots, StaffSlots, ValidSlots),
    remove_unf_intervals(Duration, ValidSlots, [(StartTime, EndTime) | _]).

intersect_all_staff_agendas([], _, []).

intersect_all_staff_agendas(StaffRequirements, _, CombinedAgenda) :-
    findall(RoleAgenda,
            (
                member((Role, _), StaffRequirements),

                normalize_role(Role, NormalizedRole),
                findall((Start, End),
                        (
                            staff(_, _, Specialization, _, Slots), 
                            normalize_role(Specialization, NormalizedSpec), 
                            sub_atom(NormalizedSpec, 0, _, 0, NormalizedRole),
                            member((Start, End), Slots) 
                        ),
                        RoleAgendaRaw),
                unique_slots(RoleAgendaRaw, RoleAgenda)
            ),
            AllAgendas),

    intersect_all_agendas(AllAgendas, CombinedAgenda).


intersect_all_agendas([], []).
intersect_all_agendas([Agenda], Agenda).
intersect_all_agendas([Agenda1, Agenda2 | Rest], CombinedAgenda) :-
    intersect_2_agendas(Agenda1, Agenda2, TempCombined),
    intersect_all_agendas([TempCombined | Rest], CombinedAgenda).

intersect_2_agendas([], _, []) :- !.
intersect_2_agendas(_, [], []) :- !.

intersect_2_agendas([(Start1, End1) | Rest1], [(Start2, End2) | Rest2], Result) :-
    MaxStart is max(Start1, Start2),
    MinEnd is min(End1, End2),
    (MaxStart < MinEnd ->
        Result = [(MaxStart, MinEnd) | RestResult],
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
    (UsedEnd =< Start ; UsedStart >= End),
    remove_time_slot(Rest, (UsedStart, UsedEnd), UpdatedRest),
    UpdatedSlots = [(Start, End) | UpdatedRest].
remove_time_slot([(Start, End) | Rest], (UsedStart, UsedEnd), UpdatedSlots) :-
    (UsedStart > Start -> UpdatedSlots = [(Start, UsedStart) | PartialUpdatedSlots] ; PartialUpdatedSlots = UpdatedSlots),
    (UsedEnd < End -> PartialUpdatedSlots = [(UsedEnd, End) | UpdatedRest] ; UpdatedRest = []),
    remove_time_slot(Rest, (UsedStart, UsedEnd), UpdatedRest).

operation_total_duration(Operation, TotalDuration) :-
    operation_data(_, Operation, PreparationTime, OperationTime, CleaningTime, _),
    TotalDuration is PreparationTime + OperationTime + CleaningTime.

normalize_role(Role, NormalizedRole) :-
    atom_codes(Role, Codes),
    maplist(to_lower, Codes, LowerCodes),
    exclude(=(32), LowerCodes, CleanCodes),
    atom_codes(NormalizedRole, CleanCodes).


unique_slots(RawSlots, UniqueSlots) :-
    sort(RawSlots, UniqueSlots).

filter_best_schedule([], []).

filter_best_schedule([(RequestId, OperationName, StartTime, EndTime) | Rest], [(RequestId, OperationName, StartTime, EndTime) | FilteredRest]) :-
    number(StartTime), 
    number(EndTime),  
    filter_best_schedule(Rest, FilteredRest).

filter_best_schedule([(_, _, StartTime, EndTime) | Rest], FilteredRest) :-
    \+ number(StartTime),
    \+ number(EndTime), 
    filter_best_schedule(Rest, FilteredRest).

preprocess_schedule([], []).

preprocess_schedule([(Id, OperationName, StartTime, EndTime) | Rest], [(QuotedId, QuotedOperationName, StartTime, EndTime) | ProcessedRest]) :-
    atom(Id), atom(OperationName), 
    quote_if_needed(Id, QuotedId),
    quote_if_needed(OperationName, QuotedOperationName),
    preprocess_schedule(Rest, ProcessedRest).

quote_if_needed(Atom, Atom) :- atom_chars(Atom, Chars), \+ member('-', Chars), !.
quote_if_needed(Atom, QuotedAtom) :- atom(Atom), !, atom_string(Atom, String), atom_string(QuotedAtom, String).

% ----------------------------------------------UTILITIES-----------------------------------------------------------------
display_schedule_and_confirm(FinalSchedule, Date, Room) :-
    display_schedule(FinalSchedule),
    write('Do you want to apply this schedule? (yes/no)'), nl,
    read(Confirmation),
    (Confirmation == yes ->
        apply_schedule(FinalSchedule, Date, Room),
        write('Schedule successfully applied.'), nl
    ;   Confirmation == no ->
        write('Schedule application canceled.'), nl
    ;   write('Invalid input. Please type "yes" or "no".'), nl, fail).

display_schedule([]).
display_schedule([(_, OperationName, StartTime, EndTime, StaffAssignments) | Rest]) :-
    write('Operation: '), write(OperationName), nl,
    write('Start Time: '), format_time(StartTime), nl,
    write('End Time: '), format_time(EndTime), nl,
    write('Assigned Staff:'), nl,
    display_staff_assignments(StaffAssignments),
    nl,
    display_schedule(Rest).

display_staff_assignments([]).
display_staff_assignments([(Role, AssignedStaff) | Rest]) :-
    write('  Role: '), write(Role), nl,
    write('    Staff: '), write(AssignedStaff), nl,
    display_staff_assignments(Rest).



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
update_requests_status([(RequestId, _, _, _, _) | Rest]) :-
    format(atom(Query), 'UPDATE OperationRequests SET Status = "ACCEPTED" WHERE RequestId = "~w"', [RequestId]),
    db_execute(Query),
    update_requests_status(Rest).


create_appointments_and_phases([], _, _).
create_appointments_and_phases([(RequestId, OperationName, StartTime, EndTime, AssignedStaff) | Rest], Date, Room) :-
    create_appointment(RequestId, OperationName, StartTime, EndTime, Date, AppointmentId),
    insert_staff_assignments(AppointmentId, AssignedStaff),
    insert_phases(OperationName, Room, AppointmentId, StartTime, Date),
    create_appointments_and_phases(Rest, Date, Room).

insert_staff_assignments(_, []).
insert_staff_assignments(AppointmentId, [(Specialization, StaffList) | Rest]) :-
    insert_staff_for_specialization(AppointmentId, Specialization, StaffList),
    insert_staff_assignments(AppointmentId, Rest).

insert_staff_for_specialization(_, _, []).
insert_staff_for_specialization(AppointmentId, Specialization, [StaffId | Rest]) :-
    format(atom(Query), 
        'INSERT INTO AllocatedStaff (AppointementId, StaffId, Specialization) VALUES ("~w", "~w", "~w")', 
        [AppointmentId, StaffId, Specialization]),
    db_execute(Query),
    insert_staff_for_specialization(AppointmentId, Specialization, Rest).



create_appointment(RequestId, _, StartTime, _, Date, AppointmentId) :-
    generate_random_string(AppointmentId),
    format_time(StartTime, StartFormatted),
    format(atom(Schedule), '~w ~w:00', [Date, StartFormatted]), 
    db_fetch('OperationRequests', RequestId, [Patient, Staff]),
    format(atom(Query), 
        'INSERT INTO Appointements (AppointementId, Schedule, Request, Patient, Staff) VALUES ("~w", "~w", "~w", "~w", "~w")', 
        [AppointmentId, Schedule, RequestId, Patient, Staff]), 
    db_execute(Query).



insert_phases(OperationName, Room, AppointmentId, StartTime, Date) :-
    operation_data(_, OperationName, PrepTime, SurgeryTime, CleaningTime, _),
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
    db_execute(Query).

db_execute(Query) :-
    write('Executing Query: '), write(Query), nl,
    odbc_query(my_db, Query, _).

db_fetch('OperationRequests', RequestId, [RecordNumber, StaffId]) :-
    format(atom(Query), 'SELECT RecordNumber, StaffId FROM OperationRequests WHERE RequestId = "~w"', [RequestId]),
    odbc_query(my_db, Query, row(RecordNumber, StaffId)).

generate_random_string(RandomString) :-
    uuid(RandomString).

generate_random_number(RandomNumber) :-
    random_between(100000, 999999, RandomNumber).

format_time(Minutes, FormattedTime) :-
    Hours is Minutes // 60,
    Mins is Minutes mod 60,
    format(atom(FormattedTime), '~|~`0t~d~2+:~|~`0t~d~2+', [Hours, Mins]).

assign_staff_to_schedule([], []).
assign_staff_to_schedule([(RequestId, OperationName, StartTime, EndTime) | Rest], [(RequestId, OperationName, StartTime, EndTime, AssignedStaff) | AssignedRest]) :-
    operation_data(_, OperationName, _, _, _, Requirements),
    assign_staff_for_operation(OperationName, StartTime, EndTime, Requirements, AssignedStaff),
    assign_staff_to_schedule(Rest, AssignedRest).

assign_staff_for_operation(_, _, _, [], []).
assign_staff_for_operation(OperationName, StartTime, EndTime, [(Specialization, Count) | RestRequirements], [(Specialization, AssignedStaff) | AssignedRest]) :-
    find_available_staff(Specialization, StartTime, EndTime, Count, AssignedStaff),
    assign_staff_for_operation(OperationName, StartTime, EndTime, RestRequirements, AssignedRest).

find_available_staff(Specialization, StartTime, EndTime, Count, AssignedStaff) :-
    downcase_atom(Specialization, SpecializationLower),
    findall(
        StaffId,
        (
            dbConnection:staff(StaffId, _, Spec, _, Slots),
            downcase_atom(Spec, SpecLower),
            SpecLower == SpecializationLower,
            has_time_slot(StartTime, EndTime, Slots)
        ),
        AllMatchingStaff
    ),
    list_to_set(AllMatchingStaff, UniqueStaff),
    length(UniqueStaff, AvailableCount),
    (   AvailableCount >= Count
    ->  random_select_n(UniqueStaff, Count, AssignedStaff)
    ;   fail
    ).


has_time_slot(StartTime, EndTime, Slots) :-
    member((SlotStart, SlotEnd), Slots),
    SlotStart =< StartTime,
    SlotEnd >= EndTime.


random_select_n(List, N, Selected) :-
    random_permutation(List, Shuffled),
    prefix(Shuffled, N, Selected).

prefix(List, N, Prefix) :-
    length(Prefix, N),
    append(Prefix, _, List).

% ----------------------------------------------US 6.3.3-----------------------------------------------------------------
schedule_with_greedy(Room, Date, BestSchedule, EarliestFinishTime) :-
    get_time(StartTime),
    write('DEBUG: Starting greedy scheduling...'), nl,
    % Collect operations with durations and staff requirements
    findall(operation(OpId, OpName, Duration, StaffRequirements),
            (
                operation_request(OpId, _, _, _, _, _, OpName),
                operation_total_duration(OpName, Duration),
                operation_data(_, OpName, _, _, _, StaffRequirements)
            ),
            OperationsWithRequirements),
    % Sort operations by duration
    sort(3, @=<, OperationsWithRequirements, SortedOperations),
    % Get room free slots
    format_input_to_number(Date, DateNumber),
    findall((Start, End),
            dbConnection:free_slot(Room, DateNumber, Start, End),
            RoomSlots),
    (RoomSlots == [] ->
        (write('DEBUG: No free slots available for scheduling.'), nl, fail);
        true
    ),
    % Perform greedy scheduling
    write('DEBUG: Initiating greedy scheduling...'), nl,
    greedy_schedule(SortedOperations, RoomSlots, [], BestSchedule, EarliestFinishTime),
    get_time(EndTime),
    assign_staff_to_schedule(BestSchedule, FinalSchedule),
    display_schedule_and_confirm(FinalSchedule, Date, Room),
    Duration is EndTime - StartTime,
    write('DEBUG: Schedule completed in '), write(Duration), write(' seconds.'), nl,
    write('DEBUG: Best schedule: '), write(BestSchedule), nl,
    write('DEBUG: Earliest finish time: '), write(EarliestFinishTime).



greedy_schedule([], _, BestSchedule, BestSchedule, EarliestFinishTime) :-
    findall(EndTime, member((_, _, _, EndTime), BestSchedule), EndTimes),
    max_list(EndTimes, EarliestFinishTime).

greedy_schedule([operation(OpId, OpName, Duration, StaffRequirements) | Rest], RoomSlots, CurrentSchedule, BestSchedule, EarliestFinishTime) :-
    % Compute combined staff agenda
    intersect_all_staff_agendas(StaffRequirements, _, CombinedAgenda),
    % Intersect room slots with staff agenda
    intersect_2_agendas(RoomSlots, CombinedAgenda, AvailableSlots),
    (   select((SlotStart, SlotEnd), AvailableSlots, RemainingSlots),
        SlotStart + Duration =< SlotEnd
    ->
        NewEndTime is SlotStart + Duration,
        append(CurrentSchedule, [(OpId, OpName, SlotStart, NewEndTime)], NewSchedule),
        % Update room slots
        update_slots(SlotStart, NewEndTime, SlotEnd, RemainingSlots, UpdatedSlots),
        greedy_schedule(Rest, UpdatedSlots, NewSchedule, BestSchedule, EarliestFinishTime)
    ;
        greedy_schedule(Rest, RoomSlots, CurrentSchedule, BestSchedule, EarliestFinishTime)
    ).


update_slots(_, SlotEnd, SlotEndTime, RemainingSlots, [(SlotEnd, SlotEndTime)|RemainingSlots]) :-
    SlotEnd < SlotEndTime, !.
update_slots(_, _, _, RemainingSlots, RemainingSlots) :-
    write('DEBUG: No splitting needed for remaining slots: '), write(RemainingSlots), nl.







