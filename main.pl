brute :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    write("Enter the room (e.g., 'R001'): "), nl,
    read(Room),
    write("Enter the number of operations to fetch:"), nl,
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


genetic :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    dbConnection:connect_to_database,
    dbConnection:initialize_environment,
    dbConnection:load_operation_data,
    dbConnection:load_and_fetch_operation_requests(Date, 100), % Pass the Limit to the predicate
    dbConnection:fetch_occupied_slots_all_rooms(Date, OccupiedSlots),
    dbConnection:generate_and_save_free_slots_all_rooms(Date, OccupiedSlots),
    % Start the genetic algorithm
    write("Initializing Genetic Algorithm..."), nl,
    genetic:gera, % This starts the genetic algorithm process.
    dbConnection:disconnect_from_database.



genetict :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    geneticDbData:gera(Date).

