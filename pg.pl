%countTwos(+,-)
countTwos([],0).
countTwos([(_,1)|RestSol], Counter):-
    countTwos(RestSol, Counter).
countTwos([(_,2)|RestSol], Counter):-
    countTwos(RestSol, Counter1),
    Counter is Counter1+1.


graph('g1', 4, [(1,2,2), (2,3,1), (3,4,1), (4,2,1), (1,3,2)])

