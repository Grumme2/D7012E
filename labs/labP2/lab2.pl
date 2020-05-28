%Anton Grahn
getsublistOuterLoop((A,I,J),R) :-
    length(A, Length), Length==1, sumlist(A,Sum), R=[(Sum,A,I,J)].

getsublistOuterLoop(([A|Atail],I,J),R) :- 
    Index is I+1,getsublistOuterLoop((Atail,Index,J),List2), getsublistInnerLoop(([A|Atail],I,J),List1), append(List1,List2,R).

getsublistInnerLoop((A,I,J),R) :-
    length(A, Length), Length==1, sumlist(A,Sum), R=[(Sum,A,I,J)].

getsublistInnerLoop((A,I,J),R) :-
    sumlist(A,Sum), List1=[(Sum,A,I,J)], Index is J-1,
    hainit(A,Init), getsublistInnerLoop((Init,I,Index),List2), append(List1, List2, R).


getSubLists(A,R) :-
    length(A,Length),Length==0, R=[([0],0,0,0)].

getSubLists(A,R) :-
    length(A,Length),Index is Length-1,getsublistOuterLoop((A,0,Index),List1), sort(1, @=<, List1, R).

smallestk(A,I) :-
    write("\nsize\tsublist\ti\tj\n"),getSubLists(A,Result),printk(Result,I).

printk(_,I) :-
    I==0, write("\n").

printk([A|Atail],Rec):-
    nthtuplevalue((A,0),Sum), nthtuplevalue((A,1),List), nthtuplevalue((A,2),I), nthtuplevalue((A,3),J),
    write(Sum), write("\t"), write(List),write("\t"),write(I),write("\t"),write(J),write("\n"),
    Rec2 is Rec-1,printk(Atail, Rec2).

nthtuplevalue(((Sum,_,_,_),Nr),R) :-
    Nr==0,R=Sum.

nthtuplevalue(((_,List,_,_),Nr),R) :-
    Nr==1,R=List.

nthtuplevalue(((_,_,I,_),Nr),R) :-
    Nr==2,R=I.

nthtuplevalue(((_,_,_,J),Nr),R) :-
    Nr==3,R=J.


hainit(L1, L2):-
    append(L2, [_], L1).