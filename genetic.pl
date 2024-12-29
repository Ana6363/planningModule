:- dynamic geracoes/1.
:- dynamic populacao/1.
:- dynamic prob_cruzamento/1.
:- dynamic prob_mutacao/1.

% operation_request(Id, Deadline, Priority, Patient, Staff, Status, Type).
operation_request(op1, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op2, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op3, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op4, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op5, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op6, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op7, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op8, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').
operation_request(op9, 20251128, HIGH, 00004, D202482952, PENDING, 'Shoulder Replacement Surgery').

operation_request(op10, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op11, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op12, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op13, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op14, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op15, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op16, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op17, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op18, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').
operation_request(op19, 20251128, HIGH, 00004, D202482952, PENDING, 'Knee Replacement Surgery').

% operation_data(Id, Type, Duration, Specs).
operation_data(type1, 'Knee Replacement Surgery', 150, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 3), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).
operation_data(type2, 'ACL Reconstruction Surgery', 72, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 3), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).
operation_data(type3, 'Meniscal Injury Treatment', 85, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 2), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).
operation_data(type4, 'Shoulder Replacement Surgery', 130, 
    [('NurseAnaesthetist', 1), ('InstrumentingNurse', 1), ('Orthopedics', 3), 
     ('Anaesthetist', 1), ('CirculatingNurse', 1), ('MedicalActionAssistant', 1)]).

% free_slots(Room, Start, End).
free_slots('R001', 480, 1200).
free_slots('R002', 480, 1200).
free_slots('R003', 480, 1200).
free_slots('R004', 480, 1200).
free_slots('R005', 480, 1200).
free_slots('R006', 480, 1200).

% operation_requests(NOps).
operation_requests(20).

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

gera_populacao(Pop) :-
    populacao(TamPop),
    findall(Operation, operation_request(Operation, _, _, _, _, _, _), OperationList),
    gera_populacao(TamPop, OperationList, Pop).

gera_populacao(0, _, []) :- !.
gera_populacao(TamPop, OperationList, [Ind|RestPop]) :-
    TamPop1 is TamPop - 1,
    schedule_operations(OperationList, Ind),
    gera_populacao(TamPop1, OperationList, RestPop).

schedule_operations(OperationList, Schedule) :-
    findall(Room, free_slots(Room, _, _), Rooms),
    initialize_room_schedules(Rooms, RoomSchedules),
    distribute_operations(OperationList, RoomSchedules, FinalSchedules),
    calculate_room_weights(FinalSchedules, Schedule).


initialize_room_schedules([], []).
initialize_room_schedules([Room|Rest], [room_schedule(Room, [])|RestSchedules]) :-
    initialize_room_schedules(Rest, RestSchedules).

% Distribute operations across rooms in a round-robin manner
distribute_operations([], RoomSchedules, RoomSchedules). % Base case: No operations left to distribute
distribute_operations(Ops, RoomSchedules, FinalSchedules) :-
    distribute_one_round(Ops, RoomSchedules, RemainingOps, UpdatedSchedules),
    distribute_operations(RemainingOps, UpdatedSchedules, FinalSchedules).

% Distribute one round of operations across all rooms
distribute_one_round([], RoomSchedules, [], RoomSchedules). % No operations to distribute
distribute_one_round(Ops, [room_schedule(Room, CurrentOps)|RestRooms], RemainingOps, [room_schedule(Room, NewOps)|UpdatedSchedules]) :-
    random_select(Op, Ops, RemainingOpsAfterSelect),  % Randomly pick an operation
    operation_request(Op, _, _, _, _, _, Type),
    operation_data(_, Type, Duration, _),
    room_finish_time(CurrentOps, CurrentFinishTime),
    free_slots(Room, _, RoomEnd),
    NewFinishTime is CurrentFinishTime + Duration,
    NewFinishTime =< RoomEnd,  % Operation fits
    append(CurrentOps, [Op], NewOps),
    distribute_one_round(RemainingOpsAfterSelect, RestRooms, RemainingOps, UpdatedSchedules).
distribute_one_round(Ops, [room_schedule(Room, CurrentOps)|RestRooms], RemainingOps, [room_schedule(Room, CurrentOps)|UpdatedSchedules]) :-
    distribute_one_round(Ops, RestRooms, RemainingOps, UpdatedSchedules).
distribute_one_round(Ops, [], Ops, []). % If all rooms are processed, return remaining operations


% Calculate the finish time of the current room
room_finish_time([], 480). % Start time of the room
room_finish_time([Op|Ops], FinishTime) :-
    operation_request(Op, _, _, _, _, _, Type),
    operation_data(_, Type, Duration, _),
    room_finish_time(Ops, PrevFinishTime),
    FinishTime is PrevFinishTime + Duration.

% Calculate weights for all rooms
calculate_room_weights([], []).
calculate_room_weights([room_schedule(Room, Ops)|Rest], [room_schedule(Room, Ops)*Weight|RestSchedules]) :-
    room_finish_time(Ops, FinishTime),
    Weight is FinishTime,
    calculate_room_weights(Rest, RestSchedules).


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

avalia_populacao([],[]).
avalia_populacao([Ind|Resto],[Ind*V|Resto1]):-
	avalia(Ind,V),
	avalia_populacao(Resto,Resto1).

avalia(Seq,V):-
	avalia(Seq,0,V).

avalia([],_,0).
avalia([T|Resto],Inst,V):-
	tarefa(T,Dur,Prazo,Pen),
	InstFim is Inst+Dur,
	avalia(Resto,InstFim,VResto),
	(
		(InstFim =< Prazo,!, VT is 0)
  ;
		(VT is (InstFim-Prazo)*Pen)
	),
	V is VT+VResto.

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
    cruzamento(Pop, NPop1),
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
    atribuir_pesos(Restantes, PesosAtribuidos),
    
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


gerar_pontos_cruzamento(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).

gerar_pontos_cruzamento1(P1,P2):-
	tarefas(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);(P1=P21,P2=P11)).
gerar_pontos_cruzamento1(P1,P2):-
	gerar_pontos_cruzamento1(P1,P2).


cruzamento([], []).
cruzamento([Ind*_], [Ind]).
cruzamento(Pop, NewPop) :-
    random_permutation(Pop, RandomizedPop),
    write('Randomized Population for Crossover: '), write(RandomizedPop), nl,
    cruzamento_pairs(RandomizedPop, NewPop).

cruzamento_pairs([], []).
cruzamento_pairs([Ind1*_, Ind2*_|Resto], [NInd1, NInd2|RestNew]) :-
    gerar_pontos_cruzamento(P1, P2),
    prob_cruzamento(Pcruz),
    random(0.0, 1.0, Pc),
    cruzamento_realizado(Pc, Pcruz, Ind1, Ind2, P1, P2, NInd1, NInd2),
    cruzamento_pairs(Resto, RestNew).
cruzamento_pairs([Ind*_|Resto], [Ind|RestNew]) :-
    cruzamento_pairs(Resto, RestNew).

cruzamento_realizado(Pc, Pcruz, Ind1, Ind2, P1, P2, NInd1, NInd2) :-
    Pc =< Pcruz, !,              % Perform crossover
    cruzar(Ind1, Ind2, P1, P2, NInd1),
    cruzar(Ind2, Ind1, P1, P2, NInd2).
cruzamento_realizado(_, _, Ind1, Ind2, _, _, Ind1, Ind2).

preencheh([],[]).

preencheh([_|R1],[h|R2]):-
	preencheh(R1,R2).


sublista(L1,I1,I2,L):-
	I1 < I2,!,
	sublista1(L1,I1,I2,L).

sublista(L1,I1,I2,L):-
	sublista1(L1,I2,I1,L).

sublista1([X|R1],1,1,[X|H]):-!,
	preencheh(R1,H).

sublista1([X|R1],1,N2,[X|R2]):-!,
	N3 is N2 - 1,
	sublista1(R1,1,N3,R2).

sublista1([_|R1],N1,N2,[h|R2]):-
	N3 is N1 - 1,
	N4 is N2 - 1,
	sublista1(R1,N3,N4,R2).

rotate_right(L,K,L1):-
	tarefas(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):-
	N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).


elimina([],_,[]):-!.

elimina([X|R1],L,[X|R2]):-
	not(member(X,L)),!,
	elimina(R1,L,R2).

elimina([_|R1],L,R2):-
	elimina(R1,L,R2).

insere([],L,_,L):-!.
insere([X|R],L,N,L2):-
	tarefas(T),
	((N>T,!,N1 is N mod T);N1 = N),
	insere1(X,N1,L,L1),
	N2 is N + 1,
	insere(R,L1,N2,L2).


insere1(X,1,L,[X|L]):-!.
insere1(X,N,[Y|L],[Y|L1]):-
	N1 is N-1,
	insere1(X,N1,L,L1).

cruzar(Ind1,Ind2,P1,P2,NInd11):-
	sublista(Ind1,P1,P2,Sub1),
	tarefas(NumT),
	R is NumT-P2,
	rotate_right(Ind2,R,Ind21),
	elimina(Ind21,Sub1,Sub2),
	P3 is P2 + 1,
	insere(Sub2,Sub1,P3,NInd1),
	eliminah(NInd1,NInd11).


eliminah([],[]).

eliminah([h|R1],R2):-!,
	eliminah(R1,R2).

eliminah([X|R1],[X|R2]):-
	eliminah(R1,R2).

mutacao([],[]).
mutacao([Ind|Rest],[NInd|Rest1]):-
	prob_mutacao(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutacao(Rest,Rest1).

mutacao1(Ind,NInd):-
	gerar_pontos_cruzamento(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).

	test_gera_populacao :-
    % Set up required dynamic facts
    asserta(populacao(1)), % Generate a single population for simplicity
    findall(Operation, operation_request(Operation, _, _, _, _, _, _), OperationList),
    gera_populacao(1, OperationList, Pop), % Call the population generation
    write('Generated Population: '), nl,
    write(Pop), nl.
