:- dynamic geracoes/1.
:- dynamic populacao/1.
:- dynamic prob_cruzamento/1.
:- dynamic prob_mutacao/1.
:- dynamic free_slots/3.
:- dynamic original_free_slots/3.

% operation_request(Id, Deadline, Priority, Patient, Staff, Status, Type).
operation_request(op1, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op2, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op3, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op4, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op5, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op6, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op7, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op8, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op9, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').

operation_request(op10, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op11, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op12, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op13, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op14, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op15, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op16, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op17, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op18, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op19, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').

operation_request(op20, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op21, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op22, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op23, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op24, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op25, 20251128, 'LOW', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op26, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op27, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op28, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op29, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op30, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').

operation_request(op31, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op32, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op33, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op34, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op35, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op36, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op37, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op38, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op39, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op40, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Knee Replacement Surgery').

operation_request(op41, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op42, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op43, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op44, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op45, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op46, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op47, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op48, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op49, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op50, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Knee Replacement Surgery').


operation_request(op51, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op52, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op53, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op54, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op55, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op56, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op57, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op58, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op59, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').

operation_request(op60, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op61, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op62, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op63, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op64, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op65, 20251128, 'LOW', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op66, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op67, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op68, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op69, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op70, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').

% operation_request(Id, Deadline, Priority, Patient, Staff, Status, Type).
operation_request(op71, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op72, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op73, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op74, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op75, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op76, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op77, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op78, 20251128, 'HIGH', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op79, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').

operation_request(op80, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op81, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op82, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op83, 20251128, 'LOW', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op84, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op85, 20251128, 'LOW', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op86, 20251128, 'LOW', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op87, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op88, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op89, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op90, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').

operation_request(op91, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op92, 20251128, 'LOW', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op93, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op94, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op95, 20251128, 'LOW', 00004, D202482952, PENDING, 'Meniscal Injury Treatment').
operation_request(op96, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op97, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op98, 20251128, 'MEDIUM', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op99, 20251128, 'LOW', 00004, D202482952, PENDING, 'ACL Reconstruction Surgery').
operation_request(op100, 20251128, 'HIGH', 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').


% operation_data(Id, Type, Duration, Specs).
operation_data(type1, 'Knee Replacement Surgery', 120, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 3), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).
operation_data(type2, 'ACL Reconstruction Surgery', 80, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 3), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).
operation_data(type3, 'Meniscal Injury Treatment', 65, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 2), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).
operation_data(type4, 'Shoulder Replacement Surgery', 130, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 3), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).


% free_slots(Room, Start, End).
free_slots('R001', 480, 1080).
free_slots('R002', 780, 1200).
free_slots('R003', 580, 1100).
free_slots('R004', 880, 1200).
free_slots('R005', 480, 1000).
free_slots('R006', 1000, 1200).


% Reset free_slots/3 to the original values only when load_original_free_slots/0 is called
load_original_free_slots :-
    retractall(free_slots(_, _, _)),  % Clear all altered free_slots/3 facts
    assert(free_slots('R001', 480, 1080)),
    assert(free_slots('R002', 780, 1200)),
    assert(free_slots('R003', 580, 1100)),
    assert(free_slots('R004', 880, 1200)), 
    assert(free_slots('R005', 480, 1000)),
    assert(free_slots('R006', 1000, 1200)).

% parameteriza  o
inicializa:-write('Numero de novas Geracoes: '),read(NG), 			(retract(geracoes(_));true), asserta(geracoes(NG)),
	write('Dimensao da Populacao: '),read(DP),
	(retract(populacao(_));true), asserta(populacao(DP)),
	write('Probabilidade de Cruzamento (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_cruzamento(_));true), 	asserta(prob_cruzamento(PC)),
	write('Probabilidade de Mutacao (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutacao(_));true), asserta(prob_mutacao(PM)).


gera:-
	inicializa,
	gera_populacao(Pop),
	write('Pop='),write(Pop),nl,
	avalia_populacao(Pop,PopAv),
	write('PopAv='),write(PopAv),nl,
	ordena_populacao(PopAv,PopOrd),
	geracoes(NG),
	gera_geracao(0,NG,PopOrd).

% Example of generating multiple populations
gera_populacao(Pop) :-
    populacao(TamPop),
    findall(Operation, operation_request(Operation, _, _, _, _, _, _), OperationList),
    load_original_free_slots,  % Load original free slots before generating population
    gera_populacao(TamPop, OperationList, [], Pop).

gera_populacao(0, _, Accumulated, Accumulated) :- !.
gera_populacao(TamPop, OperationList, Accumulated, Pop) :-
    TamPop1 is TamPop - 1,
    load_original_free_slots, 
    schedule_operations(OperationList, Schedule),
    gera_populacao(TamPop1, OperationList, [Schedule|Accumulated], Pop).

% Helper predicate to create room assignments
create_assignment(Room, Room-[]).

% Base case: No more operations to assign
round_robin_assign([], _, Accumulated, Accumulated) :-
nl.

% Assign a random operation to a room or skip it if no room can fit
round_robin_assign(Operations, Rooms, Accumulated, FinalAssignments) :-
    random_member(Operation, Operations), % Pick a random operation
    op_duration(Operation, Duration,_),
    attempt_assignment(Operation, Duration, Rooms, Accumulated, UpdatedAssignments),
    % Remove the assigned operation from the list
    select(Operation, Operations, RemainingOperations),
    round_robin_assign(RemainingOperations, Rooms, UpdatedAssignments, FinalAssignments).


% Attempt to assign an operation to the current room and continue
attempt_assignment(Operation, Duration, [Room|RemainingRooms], Accumulated, FinalAssignments) :-
    assign_to_room(Room, Duration),
    select(Room-OperationsList, Accumulated, RemainingAccumulated),
    append(OperationsList, [Operation], UpdatedOperationsList),
    FinalAssignments = [Room-UpdatedOperationsList|RemainingAccumulated],
    write('Assigned operation: '), write(Operation), write(' to room: '), write(Room), nl.

% If assignment to the current room fails, recursively try the next room
attempt_assignment(Operation, Duration, [Room|RemainingRooms], Accumulated, FinalAssignments) :-
    \+ assign_to_room(Room, Duration), % Fails to assign
    attempt_assignment(Operation, Duration, RemainingRooms, Accumulated, FinalAssignments).

% Base case: No rooms left to try, skip the operation
attempt_assignment(_, _, [], Accumulated, Accumulated) :-
    write('Operation could not be assigned'), nl.

% Retrieve the duration of an operation by matching its type
op_duration(Operation, Duration, Priority) :-
    operation_request(Operation, _, Priority, _, _, _, Type),
    operation_data(_, Type, Duration, _).

% Predicate to assign an operation to a room if it fits within the available time slot
assign_to_room(Room, Duration) :-
    free_slots(Room, Start, End),
    NewStart is Start + Duration,
    NewStart =< End,
    retract(free_slots(Room, Start, End)),
    assert(free_slots(Room, NewStart, End)).

% Predicate to generate a schedule by stacking operations in rooms
schedule_operations(OperationList, Schedule) :-
    findall(Room, free_slots(Room, _, _), Rooms), % Find available rooms
    maplist(create_assignment, Rooms, InitialAssignments), % Initialize room assignments
    round_robin_assign(OperationList, Rooms, InitialAssignments, UnfilteredSchedule),
    clean_schedule(UnfilteredSchedule, Schedule), % Clean the schedule by removing empty assignments
    write('Final schedule: '), write(Schedule), nl.
  

% Predicate to clean the schedule and remove empty room-operation pairs
clean_schedule([], []).
clean_schedule([Room-[]|Rest], CleanedRest) :-
    clean_schedule(Rest, CleanedRest).
clean_schedule([Room-Operations|Rest], [Room-Operations|CleanedRest]) :-
    Operations \= [],
    clean_schedule(Rest, CleanedRest).

gera_individuo([G],1,[G]):-!.

gera_individuo(ListaTarefas,NumT,[G|Resto]):-
	NumTemp is NumT + 1, % To use with random
	random(1,NumTemp,N),
	retira(N,ListaTarefas,G,NovaLista),
	NumT1 is NumT-1,
	gera_individuo(NovaLista,NumT1,Resto).

retira(1,[G|Resto],G,Resto).
retira(N,[G1|Resto],G,[G1|Resto1]):-
	N1 is N-1,
	retira(N1,Resto,G,Resto1).

% Evaluate the population by summing the weights of each room and appending the weight to each element
avalia_populacao([], []).  % Base case for empty population
avalia_populacao([Ind|Rest], [Ind*Weight|RestWithWeights]) :-
    avalia_schedule(Ind, Weight),   % Step 1: Calculate weight for the individual schedule
    avalia_populacao(Rest, RestWithWeights).  % Step 2: Process the rest of the population

% Calculate the total weight for a single schedule
avalia_schedule(Schedule, FinalWeight) :-
    collect_all_operations(Schedule, AllOps),           % Collect all operations from all rooms
    check_duplicates(AllOps, PenaltyFactor),            % Check for duplicates in the aggregated list
    calculate_schedule_weight(Schedule, BaseWeight),    % Calculate the total weight for all rooms
    FinalWeight is BaseWeight * PenaltyFactor,          % Apply the penalty factor
    write('Final Weight for Schedule: '), write(FinalWeight), nl.

% Collect all operations from all rooms into a single list
collect_all_operations([], []).  % Base case: No rooms left
collect_all_operations([_-Ops|Rest], AllOps) :-
    collect_all_operations(Rest, RestOps),  % Recursively collect operations from the rest of the rooms
    append(Ops, RestOps, AllOps).           % Append operations from the current room to the aggregated list

% Check for duplicates in the aggregated list and calculate the penalty factor
check_duplicates(AllOps, PenaltyFactor) :-
    write('Checking duplicates across all rooms: '), write(AllOps), nl,  % Debug: Show all operations
    sort(AllOps, UniqueOps),  % Get the unique operations
    write('Unique Operations: '), write(UniqueOps), nl,  % Debug: Show unique operations
    length(AllOps, TotalOps),  % Total operations
    length(UniqueOps, UniqueOpsCount),  % Unique operations count
    DuplicateCount is TotalOps - UniqueOpsCount,
    write('Duplicate Count: '), write(DuplicateCount), nl,  % Debug: Show duplicate count
    PenaltyFactor is 1.0 + (0.1 * DuplicateCount),  % Apply proportional penalty
    write('Penalty Factor: '), write(PenaltyFactor), nl.

% Calculate the total weight for all rooms in a schedule
calculate_schedule_weight([], 0).  % Base case: No rooms left
calculate_schedule_weight([Room-Ops|Rest], TotalWeight) :-
    calculate_room_weight(Room, Ops, RoomWeight, _RoomOps),  % Calculate weight for the current room
    calculate_schedule_weight(Rest, RestWeight),            % Recursively calculate weight for the rest of the rooms
    TotalWeight is RoomWeight + RestWeight.             

calculate_room_weight(Room, Ops, FinalWeight, RoomOps) :-
    load_original_free_slots, 
    free_slots(Room, Start, End),  % Get the start and end time for the room
    calculate_room_weight(Ops, Start, 1, InterimTotalWeight, FinalMultiplier, [], RoomOps),  % Calculate weight and collect ops
    (InterimTotalWeight > End ->  % Check if the total weight exceeds the end time
        AdjustedWeight is InterimTotalWeight * 2,
        FinalWeight is AdjustedWeight * FinalMultiplier  % Apply the multiplier
    ;
        FinalWeight is InterimTotalWeight * FinalMultiplier  % Apply the multiplier without adjustment
    ).

% Helper predicate to accumulate the total weight, collect operations, and calculate the final multiplier
calculate_room_weight([], Accumulator, Multiplier, Accumulator, Multiplier, RoomOps, RoomOps).  % When no operations left, return accumulated value
calculate_room_weight([Op|Rest], Accumulator, CurrentMultiplier, TotalWeight, FinalMultiplier, OpsSoFar, RoomOps) :-
    op_duration(Op, Duration, Priority),  % Get the duration and priority of the current operation
    NewAccumulator is Accumulator + Duration,  % Add duration to the accumulator
    adjust_multiplier(CurrentMultiplier, Priority, UpdatedMultiplier),  % Update the multiplier based on priority
    calculate_room_weight(Rest, NewAccumulator, UpdatedMultiplier, TotalWeight, FinalMultiplier, [Op|OpsSoFar], RoomOps).  % Add Op to the list and continue




% Adjust the multiplier based on the priority
adjust_multiplier(CurrentMultiplier, 'HIGH', UpdatedMultiplier) :-
    UpdatedMultiplier is CurrentMultiplier - 0.03.
adjust_multiplier(CurrentMultiplier, 'MEDIUM', UpdatedMultiplier) :-
    UpdatedMultiplier is CurrentMultiplier - 0.015.
adjust_multiplier(CurrentMultiplier, 'LOW', UpdatedMultiplier) :-
    UpdatedMultiplier is CurrentMultiplier.  % No change for LOW priority


ordena_populacao(PopAv,PopAvOrd):-
	bsort(PopAv,PopAvOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
	bsort(Xs,Zs),
	btroca([X|Zs],Ys).


btroca([X],[X]):-!.

btroca([X*VX,Y*VY|L1],[Y*VY|L2]):-
	VX>VY,!,
	btroca([X*VX|L1],L2).

btroca([X|L1],[X|L2]):-btroca(L1,L2).

% Base case for stopping the recursion
gera_geracao(G, G, Pop):- 
    write('Final Geracao '), write(G), write(':'), nl, write(Pop), nl.

gera_geracao(N, G, Pop) :-
    N < G,
    write('Processing Generation '), write(N), nl,
    write('Current Population: '), write(Pop), nl,
    strip_weights(Pop,StrippedPop),
    cruzamento(StrippedPop, NPop1),
    write('After Crossover: '), write(NPop1), nl,
    mutacao(NPop1, NPop),
    write('After Mutation: '), write(NPop), nl,
    avalia_populacao(NPop, NPopAv),
    write('After Evaluation: '), write(NPopAv), nl,
    combina_populacoes(Pop, NPopAv, NPopOrd),
    write('After Combining Populations: '), write(NPopOrd), nl,
    N1 is N + 1,
    gera_geracao(N1, G, NPopOrd).

combina_populacoes(Pop1, Pop2, PopFinal) :-
    append(Pop1, Pop2, PopCombined),         % Merge Pop e descendentes
    sort(PopCombined, PopUnique),            % Remove duplicados
    ordena_populacao(PopUnique, PopOrd),     % Sort by fitness (ascending)
    populacao(TamPop),                       % Total Pop
    P is max(1, TamPop // 5),                % Top `P` (20% of Pop)
    prefix(Elite, PopOrd),                   % Seleciona top `P` as elite
    length(Elite, P),                        % verifica q P elite existem
    subtract(PopOrd, Elite, Restantes),      % Retira os elite 
    
    % calcular pesos dos restantes
    atribuir_pesos(Restantes, PesosAtribuidos), % MUITO PROVAVEL AQUI
    
    % Sort
    sort(2, @=<, PesosAtribuidos, PesosOrdenados),
    write('Pesos Ordenados: '), write(PesosOrdenados), nl,
    
    % Select top N-P
    RestanteTam is TamPop - P,
    prefix(PesosSelecionados, PesosOrdenados),
    length(PesosSelecionados, RestanteTam), % Ensure correct count
    reatribuir_avaliacoes(PesosSelecionados, Restantes, Selecionados), % Reassign original evaluations
    
    % Combina elite e N-P
    append(Elite, Selecionados, Combinado),

    % Sort final
    ordena_populacao(Combinado, PopFinal),
    length(PopFinal, TamPop).

atribuir_pesos([], []).
atribuir_pesos([Ind*Eval|Rest], [Ind*PesoArredondado|RestPesos]) :-
    random(0.0, 1.0, FatorAleatorio),
    Peso is Eval * FatorAleatorio,
    format(atom(PesoStr), '~2f', [Peso]),
    atom_number(PesoStr, PesoArredondado),   
    atribuir_pesos(Rest, RestPesos).

reatribuir_avaliacoes([], _, []).
reatribuir_avaliacoes([Ind*_|RestPesos], Restantes, [Ind*Eval|RestReatribuido]) :-
    member(Ind*Eval, Restantes),
    reatribuir_avaliacoes(RestPesos, Restantes, RestReatribuido).

% Perform crossover for the entire population
cruzamento([], []).  % No individuals left
cruzamento([Ind], [Ind]).  % Only one individual left, no crossover
cruzamento(Pop, NewPop) :-
    random_permutation(Pop, RandomizedPop),  % Shuffle the population once
    write('Randomized Population for Crossover: '), write(RandomizedPop), nl,
    cruzamento_pairs(RandomizedPop, NewPop).  % Perform crossover on pairs

% Process crossover pairs with probability
cruzamento_pairs([], []).  % No pairs left
cruzamento_pairs([Ind1, Ind2 | Rest], [NInd1, NInd2 | RestNew]) :-
    write('Crossover pair: '), write(Ind1), write(' and '), write(Ind2), nl,
    prob_cruzamento(ProbCruz),  % Retrieve the crossover probability
    random(0.0, 1.0, Pc),  % Generate a random probability
    (Pc =< ProbCruz ->  % Check if crossover should occur
        cruzamento_individual(Ind1, Ind2, NInd1, NInd2),
        write('Crossover occurred for pair: '), write(Ind1), write(' and '), write(Ind2), nl
    ;
        NInd1 = Ind1,
        NInd2 = Ind2,
        write('No crossover for pair: '), write(Ind1), write(' and '), write(Ind2), nl
    ),
    cruzamento_pairs(Rest, RestNew).  % Continue with the rest of the pairs
cruzamento_pairs([Ind], [Ind]).  % If an odd number of individuals, keep the last one unchanged


% Perform crossover for the individual pair
cruzamento_individual(Ind1, Ind2, NInd1, NInd2) :-
    cruzamento_room(Ind1, Ind2, NInd1, NInd2).  % Perform crossover at the room level

% Perform crossover for each room
cruzamento_room([], [], [], []).  % No rooms left
cruzamento_room([R1-Op1 | Rest1], [R2-Op2 | Rest2], [R1-NewOp1 | RestNew1], [R2-NewOp2 | RestNew2]) :-
    write('Performing crossover for room: '), write(R1), nl,
    length(Op1, Len1),
    length(Op2, Len2),
    MinLen is min(Len1, Len2),  % Use the smaller length to ensure valid points
    NumSwaps is MinLen // 2,    % Determine the number of swaps (half the operations)
    write('Number of swaps for room: '), write(NumSwaps), nl,
    swap_multiple_points(Op1, Op2, NumSwaps, NewOp1, NewOp2),  % Perform multiple swaps
    cruzamento_room(Rest1, Rest2, RestNew1, RestNew2).  % Continue for the rest of the rooms

% Swap multiple points between two operation lists
swap_multiple_points(Op1, Op2, 0, Op1, Op2) :- !.  % Base case: no swaps left
swap_multiple_points(Op1, Op2, NumSwaps, NewOp1, NewOp2) :-
    length(Op1, Len1),
    length(Op2, Len2),
    MinLen is min(Len1, Len2),  % Compute the smaller length
    random_between(1, MinLen, Point),  % Select a random valid point
    nth1(Point, Op1, Elem1, Rest1),  % Extract the element at Point from Op1
    nth1(Point, Op2, Elem2, Rest2),  % Extract the element at Point from Op2
    handle_swap(Op1, Op2, Elem1, Elem2, Point, Rest1, Rest2, NumSwaps, NewOp1, NewOp2).

% Handle swap: Check for duplicates and either skip or perform the swap
handle_swap(Op1, Op2, Elem1, Elem2, Point, Rest1, Rest2, NumSwaps, NewOp1, NewOp2) :-
    (member(Elem2, Op1) ; member(Elem1, Op2)),  % Check if duplicates exist
    write('Duplicate detected, skipping swap for point: '), write(Point), nl,
    RemainingSwaps is NumSwaps - 1,  % Decrement the swap counter
    swap_multiple_points(Op1, Op2, RemainingSwaps, NewOp1, NewOp2).

handle_swap(Op1, Op2, Elem1, Elem2, _Point, Rest1, Rest2, NumSwaps, NewOp1, NewOp2) :-
    \+ member(Elem2, Op1),
    \+ member(Elem1, Op2),  % Ensure no duplicates
    nth1(Point, TempOp1, Elem2, Rest1),  % Replace in Op1 with Elem2
    nth1(Point, TempOp2, Elem1, Rest2),  % Replace in Op2 with Elem1
    RemainingSwaps is NumSwaps - 1,  % Decrement the swap counter
    swap_multiple_points(TempOp1, TempOp2, RemainingSwaps, NewOp1, NewOp2).


strip_weights([], []). % Base case: empty list
strip_weights([Ind*Weight | Rest], [Ind | StrippedRest]) :-
    strip_weights(Rest, StrippedRest). % Recursively strip the weights


% Perform mutation for the entire population
mutacao([], []).
mutacao([Ind|Rest], [NInd|Rest1]) :-
    prob_mutacao(Pmut),  % Retrieve mutation probability
    random(0.0, 1.0, Pm),  % Generate a random number
    ((Pm < Pmut, !, mutacao_individual(Ind, NInd)) ; NInd = Ind),  % Apply mutation conditionally
    mutacao(Rest, Rest1).

% Perform mutation for a single individual
mutacao_individual(Ind, NInd) :-
    pick_two_random_rooms(Ind, Room1, Room2, Rest),
    Room1 = R1Key-R1Ops,
    Room2 = R2Key-R2Ops,
    write('Mutacao: Rooms selected for mutation: '), write(R1Key), write(', '), write(R2Key), nl,
    length(R1Ops, Len1),
    length(R2Ops, Len2),
    MinLen is min(Len1, Len2),  % Determine the smaller length
    NumSwaps is MinLen // 2,  % Calculate number of swaps (half the operations)
    write('Number of swaps for mutation: '), write(NumSwaps), nl,
    swap_multiple_points(R1Ops, R2Ops, NumSwaps, NewR1Ops, NewR2Ops),  % Swap operations within the rooms
    replace_room(Ind, R1Key, NewR1Ops, TempInd),
    replace_room(TempInd, R2Key, NewR2Ops, NInd).

% Replace the operations of a room in the individual
replace_room([RoomKey-_|Ind], RoomKey, NewOps, [RoomKey-NewOps|Ind]).
replace_room([Room|Ind], RoomKey, NewOps, [Room|NInd]) :-
    replace_room(Ind, RoomKey, NewOps, NInd).

% Select two distinct rooms from the same individual
pick_two_random_rooms(Ind, Room1, Room2, Rest) :-
    random_select(Room1, Ind, Temp),  % Pick first room
    random_select(Room2, Temp, Rest).  % Pick second room

% Select two distinct rooms from the same individual
pick_two_random_rooms(Ind, Room1, Room2, Rest) :-
    random_select(Room1, Ind, Temp),  % Pick first room
    random_select(Room2, Temp, Rest).  % Pick second room

test_gera_populacao :-
    % Retrieve initial free_slots facts
    findall(free_slots(Room, Start, End), free_slots(Room, Start, End), FreeSlotsList),
    write('Initial Free Slots: '), nl,
    write(FreeSlotsList), nl,

    % Initialize free slots
    load_original_free_slots,

    % Set up required dynamic facts
    asserta(populacao(2)), % Generate a single population for simplicity
    findall(Operation, operation_request(Operation, _, _, _, _, _, _), OperationList),
    gera_populacao(Pop), % Call the population generation
    write('Generated Population: '), nl,
    write(Pop), nl,
    avalia_populacao(Pop, AvPop),
    write('Evaluated Pop='), write(AvPop), nl,
    ordena_populacao(AvPop, OrdPop),
    write('Order Pop='), write(OrdPop), nl,
    write('Probabilidade de Cruzamento (%):'), read(P1),
    PC is P1 / 100,
    (retract(prob_cruzamento(_)) ; true), asserta(prob_cruzamento(PC)),
    cruzamento(Pop, NPop1),
    write('Crossover Pop='), write(NPop1), nl,
    write('Probabilidade de Mutacao (%):'), read(P2),
    PM is P2 / 100,
    (retract(prob_mutacao(_)) ; true), asserta(prob_mutacao(PM)),
    mutacao(NPop1, MutatedPop),
    avalia_populacao(MutatedPop, AvlPop),
    write('Avl Mutated Pop='), nl, % Add a line break for better readability
    write(AvlPop), nl,
    combina_populacoes(AvPop, AvlPop, NPopOrd),
    write('Combined Pop='), nl, % Add a line break for better readability
    write(NPopOrd), nl.

