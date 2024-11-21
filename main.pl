main :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    write("Enter the room (e.g., 'R001'): "), nl,
    read(Room),
    dbConnection:connect_to_database,
    dbConnection:initialize_environment,
    dbConnection:load_operation_data,
    dbConnection:load_and_fetch_operation_requests(Date),
    dbConnection:fetch_occupied_slots(Room, Date, OccupiedSlots),
    dbConnection:generate_and_save_free_slots(Room, Date, OccupiedSlots),
    dbConnection:fetch_staff_with_slots(Date),
    dbConnection:schedule_all_surgeries(Room, Date, BestSchedule, EarliestFinishTime),
    dbConnection:disconnect_from_database.


% Remove duplicates and keep only unique slots
unique_slots(RawSlots, UniqueSlots) :-
    sort(RawSlots, UniqueSlots). % Sort and remove duplicates

