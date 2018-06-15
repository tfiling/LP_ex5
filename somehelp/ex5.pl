%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%** Bee & satSolver - Consult Modules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Consult auxRunExpr module
:- use_module('bee/bApplications/auxs/auxRunExpr',[runExprMin/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helper Functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialize bee numbers
declareNums([],_,_,Cs-Cs).
declareNums([Num|Nums],LB,UB,[new_int(Num,LB,UB)|CsH]-CsT):-
	var(Num),!,
    declareNums(Nums,LB,UB,CsH-CsT),
    !.
declareNums([_|Nums],LB,UB,C):-
    declareNums(Nums,LB,UB,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%% Task:  Solve Radio (100%) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  radio(Graph,Min,Sol)/3 -
%  given a graph returns the minimum
%  number of strong transmitters required to obtain full connectivity 
%  and a list of pairs representing an assignment of transmitters 
%  to vertices of the form (v,t).
%  Mode: solveAll(+,-,-).
radio(Graph,Min,Sol) :-
	solve(Graph,Sol),
	countOnes(Sol,Min). % Gets The Number of 1 -> The number of strong transmitors
		
solve(Instance,Solution) :-
	runExprMin(Instance,Solution,
				encode,
				decode,
				verify).

% Gets The Number of 1 -> The number of strong transmitors
countOnes([],0).
countOnes([(_,1)|Rest],Min) :-
	countOnes(Rest,Min).
countOnes([(_,2)|Rest],Min) :-
	countOnes(Rest,Min1),
	Min is Min1+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Radio - Encode %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode(graph(_,N,Weights),BooleanList,Min,Constraints) :-
	findall(A,between(1,N,A),Vertices),                           % Intialize verticses list
	length(BooleanList,N),                                        % Intialize map - BooleanList
	encode_paths(N,Weights,BooleanList,Vertices,Constraints-CT1), % Encode strongly connected graph
	sum_radios(N,BooleanList,	Min,CT1-[]).                         % Encode the number of strong transmiters - 
																 % We require that this number will be minimized

% Encode that:
% 1. S is strongly connected
% 2. There is a path from the rest to S	
encode_paths(N,Weights,BooleanList,[S|Rest],C1-C2) :-
	encode_paths(N,Weights,BooleanList,S,Rest,C1-C2).
	
encode_paths(_,_,_,_,[],T-T).
encode_paths(N,Weights,BooleanList,V1,[V2|Rest],C1-C2) :-
	encode_path_from_s_to_t(N,Weights,BooleanList,V1,V2,C1-CT1),  % Encode valid path from V1 to V2
	encode_path_from_s_to_t(N,Weights,BooleanList,V2,V1,CT1-CT2), % Encode valid path from V2 to V1
	encode_paths(N,Weights,BooleanList,V1,Rest,CT2-C2).

% Encode valid path from S to T	
encode_path_from_s_to_t(N,Weights,BooleanList,S,T,C1-C2) :-
	length(Path,N), % Path of length N - enables self loops -> All the paths are possible  
	nth1(1,Path,S), % The first vertex is S
	nth1(N,Path,T), % The last vertex is T
	declareNums(Path, 1, N, C1-CT1), % Intialize bee numbers
	encode_valid_path(N,Weights,BooleanList,Path,CT1-C2). % Encode valid path between S to T

% Encode that path is valid	
encode_valid_path(_,_,_,[],T-T).
encode_valid_path(_,_,_,[_],T-T).
encode_valid_path(N,Weights,BooleanList,[V1,V2|Rest],C1-C2) :-
	encode_valid_edge(Weights,BooleanList,V1,V2,C1-CT1), % Encode that each neighbor pair are valid 
	encode_valid_path(N,Weights,BooleanList,[V2|Rest],CT1-C2).

		
encode_valid_edge(Weights,BooleanList,A1,A2,[int_eq_reif(A1,A2,X), % Case 1: Self loop
											bool_array_or([X|Xs])|C1]-C2) :-
	encode_valid_edge(Weights,BooleanList,A1,A2,Xs,C1-C2). 
		
encode_valid_edge([],_,_,_,[],T-T).
% Case 2: Edge of weight 1
encode_valid_edge([(V1,V2,1)|Ws],BooleanList,A1,A2,[X,XX|Xs],[int_eq_reif(V1,A1,X1),
											                 int_eq_reif(V2,A2,X2),
											                 bool_and_reif(X1,X2,X),
											                 int_eq_reif(V2,A1,XX1),
											                 int_eq_reif(V1,A2,XX2),
											                 bool_and_reif(XX1,XX2,XX)
											                 |C1]-C2) :-
	encode_valid_edge(Ws,BooleanList,A1,A2,Xs,C1-C2).
% Case 3: Edge of weight 2
encode_valid_edge([(V1,V2,2)|Ws],BooleanList,A1,A2,[X,XX|Xs],[int_eq_reif(V1,A1,X1),
											                 int_eq_reif(V2,A2,X2),
											                 bool_array_and_reif([X1,X2,B1],X),
											                 int_eq_reif(V2,A1,XX1),
											                 int_eq_reif(V1,A2,XX2),
											                 bool_array_and_reif([XX1,XX2,B2],XX)
											                 |C1]-C2) :-
	nth1(V1,BooleanList,B1),
	nth1(V2,BooleanList,B2),
	encode_valid_edge(Ws,BooleanList,A1,A2,Xs,C1-C2).
 
 % Sum the number of strong transmiters 
sum_radios(N,BooleanList,Sum,[new_int(Sum,0,N),
							 bool_array_sum_eq(BooleanList,Sum)|T]-T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Radio - Decode %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	 	
% Create list of pairs : (V,T) -
% V - Vertex 
% T - transmiters 1/2
decode(Map,Solution) :-
	length(Map,N),
	findall(C,(between(1,N,I),nth1(I,Map,A),transmitter(A,T),C=(I,T)),Solution).

transmitter(-1,1).
transmitter(1,2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Radio - Verify %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Verify that the sol is valid -> The graph is strongly connected with correct assignment	 of transmiters
verify(graph(_,N,Weights),Solution) :-
	findall(C,(between(1,N,I),C=(I,_)),Solution),             % All the vertics are in the solution
	findall(I,between(1,N,I),Vertices),
	findall(T,(tupple(V1,V2,Vertices),T=(V1,V2)),Tupples),    % Create all the tupples
	verify_all_tupples_have_a_path(Tupples,Solution,Weights). % Ensure that there is a path between each tupple
	
% V1 and V2 are different members of Vertices
tupple(V1,V2,Vertices) :-
	member(V1,Vertices),
	member(V2,Vertices),
	V1 \= V2.

% For each tupple - check for a path under the assignment using dfs
verify_all_tupples_have_a_path([],_,_).
verify_all_tupples_have_a_path([(S,T)|Rest],Solution,Weights) :-
	dfs(S,T,Solution,Weights),
	verify_all_tupples_have_a_path(Rest,Solution,Weights).

% DFS is true iff there is a valid path between S and Goal regarding the solution and weights	
dfs(S,Goal,Solution,Weights) :-
	dfs_helper(S,[S],Goal,Solution,Weights).
	
dfs_helper(Goal,_,Goal,_,_) :- 
	!.
dfs_helper(S,SoFar,Goal,Solution,Weights) :-
	arc(S,Solution,Weights,S2),
	\+member(S2,SoFar),
	dfs_helper(S2,[S2|SoFar],Goal,Solution,Weights).

% a valid edge between V and S can be achived iff
%     1. the distance between V and S is 1 and V has a regular transmitter
%     2. V has a strong transmitter
arc(V,Solution,Weights,S) :-
	comb(A,B),
	member((V,A),Solution),
	(member((V,S,B),Weights) ; member((S,V,B),Weights)).
	
% The possible combinations of transmitter and weight
comb(1,1).
comb(2,1).
comb(2,2).