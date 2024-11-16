:- [dbConnection].

% Entry point for testing staff availability
main(Date) :-
    dbConnection:connect_to_database,
    format("Testing staff availability on date: ~w~n", [Date]),
    test_loaded_staff,  % Test staff loading
    dbConnection:load_available_slots(Date),  % Load agenda_staff/3 facts
    test_loaded_agenda_staff,  % Test loaded free_agenda_staff/3
    test_intersect_all_agendas(Date),  % Test shared availability
    test_slot_intersection,  % Test individual slot intersections
    dbConnection:disconnect_from_database.

% Test: Display loaded staff data
test_loaded_staff :-
    dbConnection:load_staff_data,
    findall((StaffId, LicenseNumber, Specialization, Email, Status), 
            dbConnection:staff(StaffId, LicenseNumber, Specialization, Email, Status), 
            StaffResults),
    format("Loaded Staff: ~w~n", [StaffResults]).

% Test: Display loaded free_agenda_staff/3 facts
test_loaded_agenda_staff :-
    findall((StaffId, Date, Slots), dbConnection:free_agenda_staff(StaffId, Date, Slots), AgendaResults),
    format("Loaded Free Agenda Staff: ~w~n", [AgendaResults]).

% Test: Calculate shared availability for specific staff
test_intersect_all_agendas(Date) :-
    StaffIds = ['D202410311', 'N202490837', 'N202444566'],  % Example list of staff IDs
    dbConnection:intersect_all_agendas(StaffIds, Date, Intersection),
    format("Shared Availability on ~w for ~w: ~w~n", [Date, StaffIds, Intersection]).

% Test: Intersection of individual slots
test_slot_intersection :-
    Slot1 = (480, 660),
    Slot2 = (600, 780),
    Slot3 = (700, 900),
    Slot4 = (660, 840),
    format("Testing intersection of ~w and ~w...~n", [Slot1, Slot2]),
    (   dbConnection:intersect_slots(Slot1, Slot2, Intersection1)
    ->  format("Intersection: ~w~n", [Intersection1])
    ;   format("No intersection found for ~w and ~w~n", [Slot1, Slot2])
    ),
    format("Testing intersection of ~w and ~w...~n", [Slot2, Slot3]),
    (   dbConnection:intersect_slots(Slot2, Slot3, Intersection2)
    ->  format("Intersection: ~w~n", [Intersection2])
    ;   format("No intersection found for ~w and ~w~n", [Slot2, Slot3])
    ),
    format("Testing intersection of ~w and ~w...~n", [Slot3, Slot4]),
    (   dbConnection:intersect_slots(Slot3, Slot4, Intersection3)
    ->  format("Intersection: ~w~n", [Intersection3])
    ;   format("No intersection found for ~w and ~w~n", [Slot3, Slot4])
    ).
