% main.pl
:- [dbConnection].          % Load the database connection module

% Example main predicate to test the connection and perform a task
main :-
    connect_to_database,
    test_db_connection,
    disconnect_from_database.

% Predicate to verify the connection by querying the Staff table
test_db_connection :-
    format("Fetching data from Staff table:~n"),
    odbc_query(my_db, 'SELECT StaffId, LicenseNumber, Specialization, Email, Status FROM Staff', 
               row(StaffId, LicenseNumber, Specialization, Email, Status)),
    format("StaffId: ~w, LicenseNumber: ~w, Specialization: ~w, Email: ~w, Status: ~w~n", 
           [StaffId, LicenseNumber, Specialization, Email, Status]),
    fail.
test_db_connection.
