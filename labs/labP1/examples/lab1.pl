%Anton Grahn
% state(room, steelkey place,broncekeyplace, package place)
% 	1,	2	  3		4		5		6	   7		8		9

%pick up steelkey : one hand free, robot and key in same room,  
% enter room2
%pick up brass key
% enter room 1
%drop steelkey
%enter room3
% pickup package
%enter room 1
%drop brasskey
%pickup steel key
%enter r2 
%DONE


invalid(state(_, X, X, X)) :- X == hands. % more than twol elements in hands 
checkState(State) :- not(invalid(State)).

move(state(Room,Room,A,B),
takesteelkey(Room),
Newstate) :- 
	Newstate = state(Room,hands,A,B),
	checkState(Newstate).

move(state(rone,hands,A,B),
enteroomtwo,
state(rtwo,hands,A,B) ).

move( state(Room,A,Room,B),takebroncekey(Room), Newstate) :-
	Newstate = state(Room,A,hands,B),
	checkState(Newstate).

move(state(rtwo,hands,A,B),
enterroomonefromrtwo,
state(rone,hands,A,B) ).

move(state(Room,hands,A,B),
dropsteelkey(Room),
state(Room,Room,A,B) ).

move(state(rone,A,hands,B),
enterroomthree,
state(rthree,A,hands,B) ).

move(state(Room,A,B,Room),
takepackage(Room),
Newstate) :- 
	Newstate = state(Room,A,B,hands),
	checkState(Newstate).

move(state(rthree,A,hands,B),
enterroomonefromrthree,
state(rone,A,hands,B) ).

move(state(Room,A,hands,B),
dropbroncekey(Room),
state(Room,A,Room,B) ).

canget( state(rtwo,_,_,hands),_,[done|[]]).

canget( State1,N,[Move|Trace2]):-
	N>0,
	move(State1,Move,State2),
	canget(State2,N-1,Trace2).
% test
