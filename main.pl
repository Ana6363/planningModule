main :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    write("Enter the room (e.g., 'R001'): "), nl,
    read(Room),
    dbConnection:connect_to_database,
    dbConnection:initialize_environment,
    dbConnection:load_operation_data,
    dbConnection:load_and_fetch_operation_requests(Date),
    dbConnection:fetch_occupied_slots(Room, Date, OccupiedSlots), % Fetch occupied slots
    dbConnection:generate_and_save_free_slots(Room, Date, OccupiedSlots),
    dbConnection:fetch_staff_with_slots(Date),
    write("Generating all possible schedules..."), nl,
    dbConnection:schedule_all_surgeries(Room, Date, BestSchedule, EarliestFinishTime),
    write("Best Schedule: "), write(BestSchedule), nl,
    write("Earliest Finish Time: "), write(EarliestFinishTime), nl.

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
