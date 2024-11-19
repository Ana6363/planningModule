schedule_optimal_surgeries(DateNumber, RoomId) :-
    % Clear previous scheduled surgeries for the day
    retractall(scheduled_surgery(_, RoomId, DateNumber, _, _, _)),
    % Process each operation request one by one
    forall(
        (
            sorted_operation_request(RequestId, Deadline, Priority, RecordNumber, StaffId, 'PENDING'),
            Deadline >= DateNumber % Ensure the request is valid for the date
        ),
        (
            % Step 2: Calculate total operation time
            calculate_operation_time(RequestId, TotalTime),


            % Step 3: Find earliest surgery room slot
            (find_surgery_room_slot(RoomId, DateNumber, TotalTime, SelectedSlot) ->
                (
                    % Step 4: Find required staff for the operation
                    find_required_staff(RequestId, SelectedSlot, AssignedStaff),

                    % Step 5: Mark surgery room slot as busy
                    mark_surgery_slot_busy(RoomId, DateNumber, SelectedSlot),
                    format("DEBUG: Marked slot busy for Room ~w: ~w~n", [RoomId, SelectedSlot]),

                    % Step 6: Save the scheduled surgery
                    save_scheduled_surgery(RequestId, RoomId, DateNumber, Start, End, AssignedStaff),
                    format("DEBUG: Saved scheduled surgery for Request ~w~n", [RequestId])
                ) ;
                (
                    format("DEBUG: No suitable slot found for Request ~w~n", [RequestId])
                )
            )
        )
    ),
    % Once all requests are processed, print the schedule and prompt the user
    print_scheduled_surgeries,
    prompt_user_schedule_decision(DateNumber, RoomId).



% Helper: Convert a date string or atom to a numeric format (e.g., '2024-11-18' -> 20241118)
format_date_to_number(DateInput, DateNumber) :-
    (atom(DateInput) -> atom_string(DateInput, DateString) ; DateString = DateInput),
    split_string(DateString, "-", "", [YearStr, MonthStr, DayStr]),
    atom_number(YearStr, Year),
    atom_number(MonthStr, Month),
    atom_number(DayStr, Day),
    DateNumber is Year * 10000 + Month * 100 + Day,
    format("DEBUG: DateNumber computed: ~w~n", [DateNumber]).

% Step 2: Calculate total operation time
calculate_operation_time(RequestId, TotalTime) :-
    format("DEBUG: calculate_operation_time called for RequestId: ~w~n", [RequestId]),
    operation_data(_, _, PreparationTime, SurgeryTime, CleaningTime, _),
    TotalTime is PreparationTime + SurgeryTime + CleaningTime,
    format("DEBUG: Total operation time for Request ~w: ~w minutes~n", [RequestId, TotalTime]).

% Step 3: Find earliest surgery room slot
find_surgery_room_slot(RoomId, DateNumber, TotalTime, (RoomId, DateNumber, ExactStart, ExactEnd)) :-
    format("DEBUG: Searching free slots for RoomId: ~w, Date: ~w, TotalTime: ~w~n", [RoomId, DateNumber, TotalTime]),
    free_slot(RoomId, DateNumber, Start, End),
    format("DEBUG: Checking free slot: Start: ~w, End: ~w~n", [Start, End]),
    Duration is End - Start,
    Duration >= TotalTime, % Check if the slot can fit the operation
    ExactStart = Start,
    ExactEnd is Start + TotalTime, % The end of the exact slot is calculated based on TotalTime
    !.

% Step 4: Find required staff for the operation
find_required_staff(RequestId, (RoomId, DateNumber, Start, End), AssignedStaff) :-
    operation_data(_, _, _, _, _, Specializations),
    findall(
        (Specialization, StaffIds),
        (
            member((Specialization, NeededCount), Specializations),
            find_staff_for_specialization(RequestId, Specialization, NeededCount, Start, End, StaffIds)
        ),
        AssignedStaff
    ),
    format("DEBUG: Assigned staff for Request ~w: ~w~n", [RequestId, AssignedStaff]).

% Helper: Find staff for a specific specialization
find_staff_for_specialization(RequestId, Specialization, NeededCount, Start, End, AssignedStaff) :-
    normalize_atom(Specialization, NormalizedSpecialization), % Normalize input specialization
    findall(
        StaffId,
        (
            staff(StaffId, _, StaffSpecialization, _, Slots), % Match specialization
            normalize_atom(StaffSpecialization, NormalizedStaffSpecialization), % Normalize staff specialization
            NormalizedSpecialization == NormalizedStaffSpecialization, % Ensure normalized match
            member((SlotStart, SlotEnd), Slots), % Check available slots
            SlotStart =< Start, % Slot starts before or at operation start
            SlotEnd >= End, % Slot ends after or at operation end
            \+ already_assigned_on_same_day(RequestId, StaffId, DateNumber) % Ensure staff is not already assigned
        ),
        AvailableStaff
    ),
    format("DEBUG: Available staff for Specialization ~w: ~w~n", [Specialization, AvailableStaff]),
    (   length(AvailableStaff, Count),
        Count >= NeededCount ->
        % Assign only the needed number of staff
        length(AssignedStaff, NeededCount), 
        append(AssignedStaff, _, AvailableStaff) % Take the first `NeededCount` staff
    ;   AssignedStaff = [] % No sufficient staff available
    ).

already_assigned_on_same_day(RequestId, StaffId, DateNumber) :-
    scheduled_surgery(RequestId, _, DateNumber, _, _, AssignedStaff), % Correct arity: 4 arguments
    member((_, StaffList), AssignedStaff),
    member(StaffId, StaffList).


% Normalize specialization names to handle minor differences in format
normalize_atom(Atom, NormalizedAtom) :-
    downcase_atom(Atom, NormalizedAtom). % Convert to lowercase to handle case sensitivity

normalize_atom(Input, Normalized) :-
    atom_string(Input, String), % Ensure the input is a string
    string_lower(String, Lowercase), % Convert to lowercase
    normalize_space(atom(Normalized), Lowercase). % Remove trailing/leading spaces and convert back to atom



% Step 5: Mark surgery room slot as busy
mark_surgery_slot_busy(RoomId, DateNumber, (RoomId, DateNumber, Start, End)) :-
    format("DEBUG: Marking slot as busy for RoomId: ~w, Date: ~w, Start: ~w, End: ~w~n", [RoomId, DateNumber, Start, End]),
    retract(free_slot(RoomId, DateNumber, SlotStart, SlotEnd)), % Remove existing free slot
    % Split the slot into two parts if necessary
    (
        Start > SlotStart ->
        (
            assertz(free_slot(RoomId, DateNumber, SlotStart, Start)),
            format("DEBUG: Created new free slot: RoomId: ~w, Date: ~w, Start: ~w, End: ~w~n", [RoomId, DateNumber, SlotStart, Start])
        ) 
        ; true
    ),
    (
        End < SlotEnd ->
        (
            assertz(free_slot(RoomId, DateNumber, End, SlotEnd)),
            format("DEBUG: Created new free slot: RoomId: ~w, Date: ~w, Start: ~w, End: ~w~n", [RoomId, DateNumber, End, SlotEnd])
        ) 
        ; true
    ),
    % Print all current free_slot facts for verification
    debug_print_free_slots(RoomId, DateNumber).

% Helper predicate to print all free_slot facts
debug_print_free_slots(RoomId, DateNumber) :-
    format("DEBUG: Current free slots for RoomId: ~w, Date: ~w~n", [RoomId, DateNumber]),
    forall(
        free_slot(RoomId, DateNumber, Start, End),
        format("  Free Slot - Start: ~w, End: ~w~n", [Start, End])
    ).

% Saving the scheduled surgery
save_scheduled_surgery(RequestId, RoomId, DateNumber, Start, End, AssignedStaff) :-
    format("DEBUG: Saving scheduled surgery for Request ~w in Room ~w on Date ~w from ~w to ~w with Staff ~w~n",
           [RequestId, RoomId, DateNumber, Start, End, AssignedStaff]),
    % Assert the fact into the knowledge base
    (   \+ scheduled_surgery(RequestId, RoomId, DateNumber, Start, End, AssignedStaff) % Check for duplicates
    ->  assertz(scheduled_surgery(RequestId, RoomId, DateNumber, Start, End, AssignedStaff)),
        format("DEBUG: Surgery saved as fact for Request ~w~n", [RequestId])
    ;   format("DEBUG: Surgery already exists for Request ~w, skipping save~n", [RequestId])
    ),
    % Print all stored facts for verification
    print_scheduled_surgeries.

% Helper: Print all scheduled surgeries facts
print_scheduled_surgeries :-
    format("DEBUG: All scheduled surgeries stored as facts:~n"),
    forall(
        scheduled_surgery(RequestId, RoomId, DateNumber, Start, End, AssignedStaff),
        format("Request ~w: Room ~w, Date ~w, Start ~w, End ~w, Staff ~w~n",
               [RequestId, RoomId, DateNumber, Start, End, AssignedStaff])
    ).

% Rest of the logic for scheduling surgeries and checking


prompt_user_schedule_decision(DateNumber, RoomId) :-
    format("DEBUG: Printing all scheduled surgeries before decision...~n"),
    print_scheduled_surgeries,  % Print facts before the prompt
    format("Do you want to use this schedule or generate a new one? (yes/no): "),
    read(UserInput),
    (   UserInput = yes
    ->  format("~nSchedule finalized for Room: ~w on Date: ~w.~n", [RoomId, DateNumber])
    ;   UserInput = no
    ->  format("~nGenerating a new schedule...~n"),
        schedule_optimal_surgeries(DateNumber, RoomId)
    ;   format("~nInvalid input. Please type 'yes' or 'no'.~n"),
        prompt_user_schedule_decision(DateNumber, RoomId)
    ).


