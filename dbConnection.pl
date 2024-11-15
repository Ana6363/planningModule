:- module(db_connection, [connect_to_database/0, disconnect_from_database/0]).

:- use_module(library(odbc)).

% Connect to the database
connect_to_database :-
    odbc_connect('hospitaldb', _Connection, [user('root'), password('K/C0QVM+rsI+'), alias(my_db), open(once)]).

% Disconnect from the database
disconnect_from_database :-
    odbc_disconnect(my_db).
