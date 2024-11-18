main :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    write("Enter the room (e.g., 'R001'): "), nl,
    read(Room),
    dbConnection:connect_to_database,
    dbConnection:load_operation_data,
    dbConnection:load_and_sort_operation_requests,
    dbConnection:fetch_occupied_slots(Room, Date, OccupiedSlots), % Fetch occupied slots
    dbConnection:generate_and_save_free_slots(Room, Date, OccupiedSlots).
