:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).


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




:- http_handler('/genetic', handle_genetic, []).

% Create the HTTP server
server(Port) :-						
        http_server(http_dispatch, [port(Port)]).
		
% Handler for /genetic endpoint
handle_genetic(Request) :-
    http_parameters(Request, [date(Date, [atom])]), % Extract 'date' parameter
    genetict(Date, Output), % Call the genetict predicate with the date
    format('Content-type: text/plain~n~n'),
    format('~w~n', [Output]). % Return the output


% Define the genetict predicate
genetict(Date, Output) :-
    catch( % Handle errors gracefully
        (   geneticDbData:gera(Date, Result), % Call the gera predicate in geneticDbData
            format(atom(Output), 'Result: ~w', [Result]) % Format the output
        ),
        Error,
        format(atom(Output), 'Error: ~w', [Error])
    ).

