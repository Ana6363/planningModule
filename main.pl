% main.pl
:- [dbConnection].


main(Date) :-
    dbConnection:connect_to_database,
    test_loaded_staff,  % Print loaded staff data
    dbConnection:load_available_slots(Date),  % Load agenda_staff/3 facts
    test_loaded_agenda_staff,  % Print agenda_staff/3 facts
    dbConnection:disconnect_from_database.

% Test: Display loaded staff data
test_loaded_staff :-
    dbConnection:load_staff_data,  % Load staff data
    findall((StaffId, LicenseNumber, Specialization, Email, Status), 
            staff(StaffId, LicenseNumber, Specialization, Email, Status), 
            StaffResults),
    format("Loaded Staff: ~w~n", [StaffResults]).

% Test: Display loaded free_agenda_staff/3 facts
test_loaded_agenda_staff :-
    findall((StaffId, Date, Slots), free_agenda_staff(StaffId, Date, Slots), AgendaResults),
    format("Loaded Free Agenda Staff: ~w~n", [AgendaResults]).

