/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <Anton Grahn> 
%    Student user id  : <antgra-6> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
%:- ensure_loaded('play.pl').

:- ensure_loaded('stupid.pl').
:- ensure_loaded('testboards.pl').

:- ensure_loaded('rndBoard.pl').

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	        [.,.,1,2,.,.], 
	        [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	        [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,InitialPlyr) :-
	initBoard(InitialState), InitialPlyr = 1.

% initialize(InitialState,InitialPlyr) :-
	% forcing1toDoNullMoves(InitialState), InitialPlyr = 1.

% initialize(InitialState,InitialPlyr) :-
	% rndBoardXYZ(InitialState), InitialPlyr = 1.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 


winner(State,Plyr) :-
	terminal(State),lessStones(State,Plyr).

lessStones(State,Winner):-
	nrofstones(State,1, Nrofstones1),nrofstones(State,2,Nrofstones2),
	lessStoneshlp(Nrofstones1,Nrofstones2,Winner).

lessStoneshlp(NrS1,NrS2,Winner):-
	NrS1>NrS2, Winner = 2.
lessStoneshlp(NrS1,NrS2,Winner):-
	NrS2>NrS1,Winner =1.

nrofstones(State,Plyr,Nrofstones):-
	append(State,Flatend), nrofstoneshelp(Flatend,Plyr,Nrofstones).

nrofstoneshelp(State,_,Nrofstones):-
	length(State, Int), Int == 0, Nrofstones=0.
	

nrofstoneshelp([Head|State],Plyr,Nrofstones):-
	Head==Plyr,nrofstoneshelp(State,Plyr,Nrofstonesres),Nrofstones is Nrofstonesres + 1.

nrofstoneshelp([Head|State],Plyr,Nrofstones) :-
	Head\=Plyr, nrofstoneshelp(State,Plyr,Nrofstones).
% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 



tie(State):-
	terminal(State),nrofstones(State,1,NrS1), 
	nrofstones(State,2,NrS2), NrS1==NrS2.
% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :-
	moves(1,State,MvListPlyr1),
	moves(2,State,MvListPlyr2),
	%writeln(MvListPlyr1),
	%writeln(MvListPlyr2),
	MvListPlyr1==[n],
	MvListPlyr2==[n].

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%


moves(Plyr,State,MvList):-
	(Plyr==1 -> AntiPlyr = 2; AntiPlyr = 1),
	length(State,Length),IndLength is Length-1,
	loopthroughState(Plyr,AntiPlyr,State,IndLength,0,MvListtemp),
	sort(MvListtemp,MvListsort), length(MvListsort,Lengthfin),
	(Lengthfin==0->MvList=[n],!;MvList=MvListsort,!).

loopthroughState(Plyr,AntiPlyr,State,Length,Y,MvList):-
	Length==Y,loopthroughStateinner(Plyr,AntiPlyr,State,Length,0,Y,MvList).

loopthroughState(Plyr,AntiPlyr,State,Length,Y,MvList):-
	loopthroughStateinner(Plyr,AntiPlyr,State,Length,0,Y,MvListInner),
	NewY is Y+1,
	loopthroughState(Plyr,AntiPlyr,State,Length,NewY,MvListres),
	append(MvListInner, MvListres, MvList).

loopthroughStateinner(Plyr,AntiPlyr,State,Length,X,Y,MvList):-
	Length==X, get(State,[X,Y],Value), Value\=Plyr,Value\=AntiPlyr,
	checkalldirections(Plyr,AntiPlyr,State,X,Y,Length),
	MvList=[[X,Y]].

loopthroughStateinner(_,_,_,Length,X,_,MvList):-
	Length==X,
	MvList=[].

loopthroughStateinner(Plyr,AntiPlyr,State,Length,X,Y,MvList):-
	get(State,[X,Y],Value),
	Value\=Plyr,Value\=AntiPlyr,
	NewX is X+1,
	checkalldirections(Plyr,AntiPlyr,State,X,Y,Length),
	loopthroughStateinner(Plyr,AntiPlyr,State,Length,NewX,Y,Mvlistres),
	MvListreslocal=[[X,Y]], append(MvListreslocal, Mvlistres, MvList).


loopthroughStateinner(Plyr,AntiPlyr,State,Length,X,Y,MvList):-
	NewX is X+1,
	loopthroughStateinner(Plyr,AntiPlyr,State,Length,NewX,Y,MvList).


checkalldirections(Plyr,AntiPlyr,State,X,Y,_) :- checkN(Plyr,AntiPlyr,State,X,Y).
checkalldirections(Plyr,AntiPlyr,State,X,Y,Length) :- checkNE(Plyr,AntiPlyr,State,X,Y,Length).

checkalldirections(Plyr,AntiPlyr,State,X,Y,Length) :- checkE(Plyr,AntiPlyr,State,X,Y,Length).
checkalldirections(Plyr,AntiPlyr,State,X,Y,Length) :- checkSE(Plyr,AntiPlyr,State,X,Y,Length).

checkalldirections(Plyr,AntiPlyr,State,X,Y,Length) :- checkS(Plyr,AntiPlyr,State,X,Y,Length).
checkalldirections(Plyr,AntiPlyr,State,X,Y,Length) :- checkSW(Plyr,AntiPlyr,State,X,Y,Length).

checkalldirections(Plyr,AntiPlyr,State,X,Y,_) :- checkW(Plyr,AntiPlyr,State,X,Y).
checkalldirections(Plyr,AntiPlyr,State,X,Y,_) :- checkNW(Plyr,AntiPlyr,State,X,Y).


checkN(Plyr,AntiPlyr,State,X,Y) :-
	NewY is Y-1,NewY >= 0,get(State,[X,NewY],Value),
	Value == AntiPlyr,
	checkNHlp(Plyr,AntiPlyr,State,X,NewY).

checkNHlp(Plyr,AntiPlyr,State,X,Y):-
	NewY is Y-1,NewY>=0, get(State, [X,NewY],Value),
	(Value==AntiPlyr -> checkNHlp(Plyr,AntiPlyr,State,X,NewY),!;Value == Plyr,!).


checkNE(Plyr,AntiPlyr,State,X,Y, Length) :-
	NewY is Y-1, NewX is X+1, NewY >= 0, NewX =< Length,
	get(State,[NewX,NewY],Value),Value == AntiPlyr,
	checkNEHlp(Plyr,AntiPlyr,State,NewX,NewY,Length).

checkNEHlp(Plyr,AntiPlyr,State,X,Y, Length):-
	NewY is Y-1, NewX is X+1, NewY >=0, NewX =< Length,
	get(State, [NewX,NewY],Value),
	(Value==AntiPlyr -> checkNEHlp(Plyr,AntiPlyr,State,NewX,NewY,Length),!;Value == Plyr,!).

checkE(Plyr,AntiPlyr,State,X,Y,Length) :-
	NewX is X+1,NewX =< Length,get(State,[NewX,Y],Value),
	Value == AntiPlyr,
	checkEHlp(Plyr,AntiPlyr,State,NewX,Y,Length).

checkEHlp(Plyr,AntiPlyr,State,X,Y,Length):-
	NewX is X+1,NewX=<Length, get(State, [NewX,Y],Value),
	(Value==AntiPlyr -> checkEHlp(Plyr,AntiPlyr,State,NewX,Y, Length),!;Value == Plyr,!).


checkSE(Plyr,AntiPlyr,State,X,Y, Length) :-
	NewY is Y+1, NewX is X+1, 
	NewY =< Length, NewX =< Length,
	get(State,[NewX,NewY],Value),
	Value == AntiPlyr,
	checkSEHlp(Plyr,AntiPlyr,State,NewX,NewY,Length).

checkSEHlp(Plyr,AntiPlyr,State,X,Y, Length):-
	NewY is Y+1, NewX is X+1, NewY =< Length, NewX =< Length,
	get(State, [NewX,NewY],Value),
	(Value==AntiPlyr -> checkSEHlp(Plyr,AntiPlyr,State,NewX,NewY,Length),!;Value == Plyr,!).

checkS(Plyr,AntiPlyr,State,X,Y,Length) :-
	NewY is Y+1,NewY =< Length,
	get(State,[X,NewY],Value),
	Value == AntiPlyr,
	checkSHlp(Plyr,AntiPlyr,State,X,NewY,Length).

checkSHlp(Plyr,AntiPlyr,State,X,Y,Length):-
	NewY is Y+1,NewY=<Length, get(State, [X,NewY],Value),
	(Value==AntiPlyr -> checkSHlp(Plyr,AntiPlyr,State,X,NewY,Length),!;Value == Plyr,!).

checkSW(Plyr,AntiPlyr,State,X,Y, Length) :-
	NewY is Y+1, NewX is X-1, NewX >= 0, NewY =< Length,
	get(State,[NewX,NewY],Value),Value == AntiPlyr,
	checkSWHlp(Plyr,AntiPlyr,State,NewX,NewY,Length).

checkSWHlp(Plyr,AntiPlyr,State,X,Y, Length):-
	NewY is Y+1, NewX is X-1, NewX >=0, NewY =< Length,
	get(State, [NewX,NewY],Value),
	(Value==AntiPlyr -> checkSWHlp(Plyr,AntiPlyr,State,NewX,NewY,Length),!;Value == Plyr,!).

checkW(Plyr,AntiPlyr,State,X,Y) :-
	NewX is X-1,NewX >= 0,get(State,[NewX,Y],Value),
	Value == AntiPlyr,
	checkWHlp(Plyr,AntiPlyr,State,NewX,Y).

checkWHlp(Plyr,AntiPlyr,State,X,Y):-
	NewX is X-1, NewX>=0, get(State, [NewX,Y],Value),
	(Value==AntiPlyr -> checkWHlp(Plyr,AntiPlyr,State,NewX,Y),!;Value == Plyr,!).


checkNW(Plyr,AntiPlyr,State,X,Y) :-
	NewY is Y-1, NewX is X-1, NewY >= 0, NewX >= 0,
	get(State,[NewX,NewY],Value),Value == AntiPlyr,
	checkNWHlp(Plyr,AntiPlyr,State,NewX,NewY).

checkNWHlp(Plyr,AntiPlyr,State,X,Y):-
	NewY is Y-1, NewX is X-1, NewY >=0, NewX >= 0,
	get(State, [NewX,NewY],Value),
	(Value==AntiPlyr -> checkNWHlp(Plyr,AntiPlyr,State,NewX,NewY),!;Value == Plyr,!).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(Plyr,n,State,NewState,NextPlayer):-
	(Plyr == 1 -> NextPlayer=2,!;NextPlayer=1,!),
	validmove(Plyr,State,n),
	NewState=State.
nextState(Plyr,Move,State,NewState,NextPlayer):-
	(Plyr == 1 -> NextPlayer=2,!;NextPlayer=1,!),
	validmove(Plyr,State,Move),
	executeMove(Plyr,NextPlayer,Move,State,NewState).
	
%executeMove(_,_,[],State,NewState):- NewState = State.
executeMove(Plyr,AntiPlyr,[X,Y],State,NewState):-
	length(State,Length),IndLength is Length-1,
	moveNorth(Plyr,AntiPlyr,X,Y,State,NewStateN),
	moveNorthEast(Plyr,AntiPlyr,X,Y,NewStateN,NewStateNE,IndLength),
	moveEast(Plyr,AntiPlyr,X,Y,NewStateNE,NewStateE,IndLength),
	moveSouthEast(Plyr,AntiPlyr,X,Y,NewStateE,NewStateSE,IndLength),
	moveSouth(Plyr,AntiPlyr,X,Y,NewStateSE,NewStateS,IndLength),
	moveSouthWest(Plyr,AntiPlyr,X,Y,NewStateS,NewStateSW,IndLength),
	moveWest(Plyr,AntiPlyr,X,Y,NewStateSW,NewStateW),
	moveNorthWest(Plyr,AntiPlyr,X,Y,NewStateW,NewState).
	
moveNorth(Plyr,AntiPlyr,X,Y,State,NewState):-
	checkN(Plyr,AntiPlyr,State,X,Y),
	NewY is Y-1, set(State,NState,[X,Y],Plyr),
	flipN(Plyr,AntiPlyr,X,NewY,NState,NewState).
moveNorth(_,_,_,_,State,NewState):- NewState = State.

flipN(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewY is Y-1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),
	flipN(Plyr,AntiPlyr,X, NewY,Nstate,NewState),!;NewState = State,!).

moveNorthEast(Plyr,AntiPlyr,X,Y,State,NewState,Length):-
	checkNE(Plyr,AntiPlyr,State,X,Y,Length),
	NewY is Y-1,NewX is X+1, set(State,NState,[X,Y],Plyr),
	flipNE(Plyr,AntiPlyr,NewX,NewY,NState,NewState).
moveNorthEast(_,_,_,_,State,NewState,_):- NewState = State.

flipNE(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewY is Y-1,NewX is X+1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),flipNE(Plyr,AntiPlyr,NewX, NewY,Nstate,NewState),!;NewState = State,!).


moveEast(Plyr,AntiPlyr,X,Y,State,NewState,Length):-
	checkE(Plyr,AntiPlyr,State,X,Y,Length),
	NewX is X+1, set(State,NState,[X,Y],Plyr),
	flipE(Plyr,AntiPlyr,NewX,Y,NState,NewState).
moveEast(_,_,_,_,State,NewState,_):- NewState = State.

flipE(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewX is X+1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),flipE(Plyr,AntiPlyr,NewX, Y,Nstate,NewState),!;NewState = State,!).


moveSouthEast(Plyr,AntiPlyr,X,Y,State,NewState,Length):-
	checkSE(Plyr,AntiPlyr,State,X,Y,Length),
	NewY is Y+1,NewX is X+1, set(State,NState,[X,Y],Plyr),
	flipSE(Plyr,AntiPlyr,NewX,NewY,NState,NewState).
moveSouthEast(_,_,_,_,State,NewState,_):- NewState = State.

flipSE(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewX is X+1,NewY is Y+1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),flipSE(Plyr,AntiPlyr,NewX, NewY,Nstate,NewState),!;NewState = State,!).

moveSouth(Plyr,AntiPlyr,X,Y,State,NewState,Length):-
	checkS(Plyr,AntiPlyr,State,X,Y,Length),
	NewY is Y+1, set(State,NState,[X,Y],Plyr),
	flipS(Plyr,AntiPlyr,X,NewY,NState,NewState).
moveSouth(_,_,_,_,State,NewState,_):- NewState = State.

flipS(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewY is Y+1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),flipS(Plyr,AntiPlyr,X, NewY,Nstate,NewState),!;NewState = State,!).

moveSouthWest(Plyr,AntiPlyr,X,Y,State,NewState,Length):-
	checkSW(Plyr,AntiPlyr,State,X,Y,Length),
	NewY is Y+1,NewX is X-1, set(State,NState,[X,Y],Plyr),
	flipSW(Plyr,AntiPlyr,NewX,NewY,NState,NewState).
moveSouthWest(_,_,_,_,State,NewState,_):- NewState = State.

flipSW(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewX is X-1,NewY is Y+1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),flipSW(Plyr,AntiPlyr,NewX, NewY,Nstate,NewState),!;NewState = State,!).

moveWest(Plyr,AntiPlyr,X,Y,State,NewState):-
	checkW(Plyr,AntiPlyr,State,X,Y),
	NewX is X-1, set(State,NState,[X,Y],Plyr),
	flipW(Plyr,AntiPlyr,NewX,Y,NState,NewState).
moveWest(_,_,_,_,State,NewState):- NewState = State.

flipW(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewX is X-1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),flipW(Plyr,AntiPlyr,NewX, Y,Nstate,NewState),!;NewState = State,!).

moveNorthWest(Plyr,AntiPlyr,X,Y,State,NewState):-
	checkNW(Plyr,AntiPlyr,State,X,Y),
	NewY is Y-1,NewX is X-1, set(State,NState,[X,Y],Plyr),
	flipNW(Plyr,AntiPlyr,NewX,NewY,NState,NewState).
moveNorthWest(_,_,_,_,State,NewState):- NewState = State.

flipNW(Plyr,AntiPlyr,X,Y,State,NewState):-
	get(State,[X,Y],Value),NewX is X-1,NewY is Y-1,
	(Value == AntiPlyr -> set(State,Nstate,[X,Y],Plyr),flipNW(Plyr,AntiPlyr,NewX, NewY,Nstate,NewState),!;NewState = State,!).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.


validmove(Plyr,State,n):-
	moves(Plyr, State, MvList),
	(MvList == [n]-> true;false).
validmove(Plyr,State,[X,Y]):-
	(Plyr==1->AntiPlyr = 2,!;AntiPlyr=1,!),
	length(State,Len), Length is Len-1,
	checkalldirections(Plyr,AntiPlyr,State,X,Y,Length).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.


h(State,Val):- 
	winner(State,Plyr),
	(Plyr==2->Val = -100,!;Val = 100,!).

h(State,Val):- tie(State), Val = 0.

h(State,Val):-
	nrofstones(State,1, Nrofstones1),
	nrofstones(State,2,Nrofstones2),
	Val is Nrofstones2-Nrofstones1.

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.


lowerBound(B):- B= -150.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.


upperBound(B):- B=150.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
