:- use_module('bee/bApplications/auxs/auxRunExpr',[runExprMin/5]).


%countTwos(+,-)
countTwos([],0).
countTwos([(_,1)|RestSol], Counter):-
    countTwos(RestSol, Counter).
countTwos([(_,2)|RestSol], Counter):-
    countTwos(RestSol, Counter1),
    Counter is Counter1+1.


%  radio(Graph,Min,Sol)/3 -
%  Mode: solveAll(+,-,-).
radio(Graph,Min,Sol) :-
    solveRadio(Graph,Sol),
    countTwos(Sol,Min). % Gets The Number of 2 -> The number of strong transmitors
    
solveRadio(Instance,Solution) :-
    runExprMin(Instance,Solution,
                encodeRadio,
                decodeRadio,
                verifyRadio).


encodeRadio(graph(_, N, Edges), Map, MinCs, Cs):-
    findall(V, between(1,N,V), VertList), %get vartices list
    length(Map, N),
    encode_strongly_connected_graph(N, Edges, Map, VertList, Cs - Cs1), %encode all the paths.
    sum_radios(N, Map, MinCs, Cs1- []),
    writeln("in ENCODE, map is" + Map),
    writeln("in ENCODE, MinCs is" + MinCs).

%encode_strongly_connected_graph(+, +, -, +, Cs - Cs1). 
%verify that:
%1. the grpah is strongly connected
%2. check that there is a path from V1 to V2 and vise vera, for each V2-VN in th e graph.
%that will be enough to ensure that from any V there is a path to any other V.
encode_strongly_connected_graph(N, Edges, Map, [V1 |VertList], Cs - Cs1):-
    encode_strongly_connected_graph_helper(N, Edges, Map, V1, VertList, Cs - Cs1).


encode_strongly_connected_graph_helper(_, _, _, _, [], Cs - Cs).
encode_strongly_connected_graph_helper(N, Edges, Map, V1, [V2|VertList], Cs - Cs1):-
    encode_path_from_v1_to_v2(N, Edges, Map, V1, V2, Cs - CsM),
    encode_path_from_v1_to_v2(N, Edges, Map, V2, V1, CsM - CsM1),
    encode_strongly_connected_graph_helper(N, Edges, Map, V1, VertList, CsM1 - Cs1).

%encode_path_from_v1_to_v2(+,+,-,+,+,-)
encode_path_from_v1_to_v2(N, Edges, Map, V1, Vn, Cs1 - Cs3):-
    length(Path, N),
    nth1(1, Path, V1),
    nth1(N, Path, Vn),
    declare_nums(Path, 1, N, Cs1-Cs2),
    encode_valid_path(N, Edges, Map, Path, Cs2 - Cs3). 

%declare_nums(-, +, +, -),
declare_nums([], _,_,Cs-Cs).
declare_nums([Num|Nums], Min, Max, [new_int(Num, Min, Max)|Cs1]- Cs2):-
    var(Num),
    !,
    declare_nums(Nums, Min, Max, Cs1-Cs2),
    !.
declare_nums([_|Nums], Min, Max, Cs):-
    declare_nums(Nums, Min, Max, Cs).

%Encode valid path from V1 to V2
%encode_valid_path(+, +, -, +, -).
%Even path base case
encode_valid_path(_, _, _, [], Cs-Cs).
%Odd path base case
encode_valid_path(_, _, _, [_], Cs-Cs).
encode_valid_path(N, Edges, Map, [V1, V2|Path], Cs1 - Cs3):-
    encode_valid_edge(Edges, Map, V1, V2, Cs1-Cs2),
    encode_valid_path(N, Edges, Map, [V2|Path], Cs2-Cs3).
   

%Encode that each neighbor pair are valid.
%1st case --> Self loop.
encode_valid_edge(Edges, Map, V1, V2, [int_eq_reif(V1, V2, X),
                                    bool_array_or([X|Xs])| Cs1] - Cs2):-
    encode_valid_edge_case_two(Edges, Map, V1, V2, Xs, Cs1-Cs2).

%base case of /6
encode_valid_edge_case_two([], _, _ , _, [], Cs-Cs).
%2nd case --> Edge weight 1.
encode_valid_edge_case_two([(A1, A2,1)|RestEdges],Map, V1, V2, [X,XX|XS], 
                                                    [%in this case V1 is A1
                                                     %V2 is A2
                                                     int_eq_reif(V1, A1, X1),
                                                     int_eq_reif(V2, A2, X2),
                                                     bool_and_reif(X1, X2, X),
                                                      %opposite
                                                     int_eq_reif(V1, A2, XX1),
                                                     int_eq_reif(V2, A1, XX2),
                                                     bool_and_reif(XX1, XX2, XX)
                                                     |Cs1] - Cs2):-
    encode_valid_edge_case_two(RestEdges, Map, V1, V2, XS, Cs1-Cs2).
%2nd case --> Edge weight 2.
encode_valid_edge_case_two([(A1, A2,2)|RestEdges],Map, V1, V2, [X,XX|XS],
                                                    [%in this case V1 is A1
                                                     %V2 is A2
                                                     int_eq_reif(V1, A1, X1),
                                                     int_eq_reif(V2, A2, X2),
                                                     bool_array_and_reif([X1, X2, B1],X),
                                                     %opposite
                                                     int_eq_reif(V1, A2, XX1),
                                                     int_eq_reif(V2, A1, XX2),
                                                     bool_array_and_reif([XX1, XX2, B2],XX)
                                                    |Cs1] -Cs2):-                                            
    nth1(A1, Map, B1),
    nth1(A2, Map, B2),
    encode_valid_edge_case_two(RestEdges, Map, V1, V2, XS, Cs1-Cs2).

%sum the number of STRONG transmiters.
sum_radios(N, Map, Sum, [new_int(Sum, 0, N),
                        bool_array_sum_eq(Map, Sum)| Cs] - Cs).




decodeRadio(Map, Solution):-
    writeln("DECODE -- Map is: " + Map),    
    length(Map, N),
    findall(Pair, (between(1,N,I),nth1(I, Map, A), transmiter(A, T), Pair=(I,T)),
        Solution),
    writeln("DECODE -- Sol is: " + Solution).

transmiter(-1,1).   
transmiter(1,2).



%We need to make sure that the graph is Strongly Connected.
%In other words - from each V we can arrive to another V'
verifyRadio(graph(_,N,Edges), Solution):-
    %first we verify all the Vertex are in the solution
    findall(V, (between(1,N,I), V = (I,_)), Solution),
    %create all pairs, to make sure there is path from any V to V'
    findall(I, between(1,N,I), VertList),
    findall(T, (pairs(V1, V2, VertList), T=(V1,V2)), Pairs),
    verify_all_pairs_in_path(Pairs, Solution, Edges).

pairs(V1, V2, VertList):-
    member(V1, VertList),
    member(V2, VertList),
    V1 \= V2.

%verify_all_pairs_in_path(+,+,+)
%make sure there is path from any V to V' (V,V') in Pairs
verify_all_pairs_in_path([], _, _).
verify_all_pairs_in_path([(V1,Vn)| VertList], Solution, Edges):-
    dfsWannaBe(V1, Vn, Solution, Edges),
    verify_all_pairs_in_path(VertList, Solution, Edges).

%we return true iff there is a VALID path from V1 to Vn
%with the Edges and the Solution.
dfsWannaBe(V1, Vn, Solution, Edges):-
                %[V1] stand for a path list that we'll build incremently. 
    dfsWannaBe_helper(V1,[V1], Vn, Solution, Edges).

dfsWannaBe_helper(Vn, _, Vn, _,_) :- !.

dfsWannaBe_helper(V1, OnGoingPath, Vn, Solution, Edges):-
    arc(V1, Solution, Edges, V2),
    \+ member(V2, OnGoingPath),
    dfsWannaBe_helper(V2,[V2|OnGoingPath], Vn, Solution, Edges).

%transmitter of 1 can go only with 1 weighted paths
combinations(1,1).
%transmitter of 2 can go with 1,2 weighted paths
combinations(2,1).
combinations(2,2).

%will check there is a OK path from V1 to V2 and it can be only iff
% 1. the distance between V1 and V2 is 1 and V1 has a 1 transmitter
% 2. V1 has a strong transmitter
arc(V1, Solution, Edges, V2):-
    combinations(A,B),
    member((V1,A), Solution),
    % we do that first       OR      this part. 
    (member((V1,V2,B),Edges) ; member((V2,V1,B),Edges)).

