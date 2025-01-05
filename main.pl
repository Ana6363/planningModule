:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- consult('geneticDbData.pl'). 
:- consult('genetic.pl'). 

main :-
    process_date. % Call your main predicate when the program starts

process_date :-
    open('input.txt', read, InputStream),
    read_line_to_string(InputStream, Date),
    close(InputStream),
    format("Processing date: ~w~n", [Date]),
    gera(Date, Result), % Call your main Prolog logic
    open('output.txt', write, OutputStream),
    write(OutputStream, Result),
    close(OutputStream),
    format("Result written to output.txt: ~w~n", [Result]).

% Example predicate for processing the date
genetic :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    geneticDbData:gera(Date,Result),
    write(Result).


    % Example predicate for processing the date
gerar :-
    write("Enter the date (e.g., '2024-11-17'): "), nl,
    read(Date),
    geneticDbData:gera(Date,Result),
    write(Result).

