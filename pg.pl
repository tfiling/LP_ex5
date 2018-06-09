%countTwos(+,-)
countTwos([],0).
countTwos([(_,1)|RestSol], Counter):-
    countTwos(RestSol, Counter).
countTwos([(_,2)|RestSol], Counter):-
    countTwos(RestSol, Counter1),
    Counter is Counter1+1.