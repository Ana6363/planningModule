brute :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    write("Enter the room (e.g., 'R001'): "), nl,
    read(Room),
    write("Enter the number of operations to fetch: (MAX 6)"), nl,
    read(Limit),
    dbConnection:connect_to_database,
    dbConnection:initialize_environment,
    dbConnection:load_operation_data,
    dbConnection:load_and_fetch_operation_requests(Date, Limit), % Pass the Limit to the predicate
    dbConnection:fetch_occupied_slots(Room, Date, OccupiedSlots),
    dbConnection:generate_and_save_free_slots(Room, Date, OccupiedSlots),
    dbConnection:fetch_staff_with_slots(Date),
    dbConnection:schedule_all_surgeries(Room, Date, _, _),
    dbConnection:disconnect_from_database.

greedy :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    write("Enter the room (e.g., 'R001'): "), nl,
    read(Room),
    write("Enter the number of operations to fetch: "), nl,
    read(Limit),
    dbConnection:connect_to_database,
    dbConnection:initialize_environment,
    dbConnection:load_operation_data,
    dbConnection:load_and_fetch_operation_requests(Date, Limit), % Pass the Limit to the predicate
    dbConnection:fetch_occupied_slots(Room, Date, OccupiedSlots),
    dbConnection:generate_and_save_free_slots(Room, Date, OccupiedSlots),
    dbConnection:fetch_staff_with_slots(Date),
    dbConnection:schedule_with_greedy(Room, Date, _, _),
    dbConnection:disconnect_from_database.


% Test
test_brute :-
    Date = '2024-12-17',
    Room = 'R001',
    dbConnection:connect_to_database,
    dbConnection:initialize_environment,
    dbConnection:load_operation_data,
    dbConnection:load_and_fetch_operation_requests(Date),
    dbConnection:fetch_occupied_slots(Room, Date, OccupiedSlots),
    dbConnection:generate_and_save_free_slots(Room, Date, OccupiedSlots),
    dbConnection:fetch_staff_with_slots(Date),
    dbConnection:schedule_all_surgeries(Room, Date, BestSchedule, EarliestFinishTime),
    write('Best Schedule: '), write(BestSchedule), nl,
    write('Earliest Finish Time: '), write(EarliestFinishTime), nl,
    length(BestSchedule, OperationCount),
    OperationCount >= 3,  % Ensure at least 3 operations
    EarliestFinishTime < 1200,  % Ensure finish time is before 1200
    write('Assertions passed: BestSchedule has at least 3 operations, and EarliestFinishTime is before 1200.'), nl,
    dbConnection:disconnect_from_database.
