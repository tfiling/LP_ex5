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
    encode_strongly_connected_graph(N, Edges, Map, VertList, Cs - Cs1). %encode all the paths.
    
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
    encode_strongly_connected_graph_helper(N, Edges, Map, V1, VertList, CsM - Cs1).