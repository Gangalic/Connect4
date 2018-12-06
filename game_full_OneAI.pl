%Creating pawn with 3 args that we'll use and store in "database"
:- dynamic pawn/3.

%Clear the "database"
clear :- retractall(pawn(_X,_Y,_Z)).

%Remove a certain pawn from "database"
removePawn(X) :- height(X,Height),retract(pawn(X,Height,_)).

%Return columns where we can still insert pawns
playable(X) :- between(1, 7, X), once(not(pawn(X, 6, _))).

%Create a list L with numbers of all insertable columns
playableList(L) :- findall(X, playable(X), L).
    
%Simple rules of displaying
display(X, Y) :- pawn(X, Y, red), write('O|').
display(X, Y) :- pawn(X, Y, yellow), write('.X|').
display(_, _) :- write('._|').

%Displaying the whole table
display :-
    between(1, 6, Tmp), Y is 7-Tmp, nl, write('|'),
    between(1, 7, X), not(display(X, Y)) ; true, !.

%Add pawn with (already specified Y) height at column X
add(X, Y, Color) :- 
    integer(X), X >= 1, X < 8,
    height(X, Count), Y is Count+1,
    integer(Y), Y >= 1, Y < 7,
    asserta(pawn(X, Y, Color)).
%Add pawn at good height in column X
add(X, Color) :-
    add(X, _, Color).

%Read player's choice from console
getX(X):-
    nl, write('Choose column to insert in:'), read(X).

%Calculate acceptable height
height(X, Count) :- aggregate_all(count, pawn(X, _, _), Count).

%Start normal game against AI
playVersusAI:-
    (getX(X), write('X:'), add(X, Y, yellow), ! ; write('You cannot play there! Please, try again.'), nl, playVersusAI), display, nl,     %If pawn was added we display, else we retry
    ( end(X, Y, yellow), ! ; nl, write('AI playing...'), choose_move(Xchosen, red), add(Xchosen, Ychosen, red), display, nl, 
    	(end(Xchosen, Ychosen, red), ! ; playVersusAI) ).       %Then, if we don't win AI has to make a move (play)
	
%Start normal game against random AI
playVersusAIRandom:-
    (getX(X), write('X:'), add(X, Y, yellow), ! ; write('You cannot play there! Please, try again.'), nl, playVersusAI), display, nl,     %If pawn was added we display, else we retry
    ( end(X, Y, yellow), ! ; nl, write('AI playing...'), random_between(1, 7, Xchosen), add(Xchosen, Ychosen, red), display, nl, 
    	(end(Xchosen, Ychosen, red), ! ; playVersusAIRandom) ).       %Then, if we don't win AI has to make a move (play)

%Start two AIs playing against each other
playTwoAIs(Color):-
    ( nl, write('AI '), write(Color), write(' playing...'), choose_move(Xchosen, Color), add(Xchosen, Ychosen, Color), display, nl, 
    	(end(Xchosen, Ychosen, Color), ! ; opposite(Color, NextColor), playTwoAIs(NextColor)) ).

%Start game between 2 physical players
playTwoPlayers(Color):-
    (getX(X), add(X, Y, Color), ! ; write('You cannot play there! Please, try again.'), nl, playTwoPlayers(Color)), display, nl,     %If pawn was added we display, else we abort
    (end(X, Y, Color), ! ; opposite(Color, NextColor), nl, write('---Player change---'), nl, playTwoPlayers(NextColor)).


%Calculate how many pawns of same color are there one after each other
%in the vector(not line, as it has direction) defined by (X,Y)->(X+DeltaX,Y+DeltaY)
%and giving the Count number as result
around(X, Y, DeltaX, DeltaY, Color, Count) :- 
    (
        between(1, 3, Counter), 
        NextX is DeltaX*Counter+X, NextY is DeltaY*Counter+Y,
        %check if no pawn of this color (or inexistent pawn) and decreasing the final count value by one
	%as we know the ones before worked except this one
        not(pawn(NextX, NextY, Color)),
        Count is Counter-1, !
    ) ; Count is 3, true. %sets Count to 3 if all last 3 tests of finding pawn of certain color passed

%Gives true if there is a serie of at least Lim pawns of the same color C alligned in any specific direction (without (X,Y) pawn)
contPawns(X, Y, C, Lim) :- around(X, Y, 1, 0, C, C1), around(X, Y, -1, 0, C, C2), Count is C1+C2, Count >= Lim, !. %horizontal
contPawns(X, Y, C, Lim) :- around(X, Y, 0, 1, C, C1), around(X, Y, 0, -1, C, C2), Count is C1+C2, Count >= Lim, !. %vertical
contPawns(X, Y, C, Lim) :- around(X, Y, 1, -1, C, C1), around(X, Y, -1, 1, C, C2), Count is C1+C2, Count >= Lim, !. %SE->NW diag
contPawns(X, Y, C, Lim) :- around(X, Y, 1, 1, C, C1), around(X, Y, -1, -1, C, C2), Count is C1+C2, Count >= Lim, !. %SW->NE diag

%Check if any winning combination after possible adding of (X,Y)
win(X, Y, C, 3) :-  contPawns(X, Y, C, 3).

%Change color to its opposite (red to yellow; yellow to red)
opposite(Color, ColorOpp) :- Color = yellow, ColorOpp = red, !.
opposite(Color, ColorOpp) :- Color = red, ColorOpp = yellow, !.

%Calculate the value(power) of putting a certain pawn here
%we consider that (X,Y) pawn is already added, and the withdraw is done after
value(X,Y, C, Val):- contPawns(X, Y, C, 3), Val is 1000, !.
value(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 3), Val is 500, !.
value(X,Y, C, Val):- contPawns(X, Y, C, 2), Val is 100, !.
value(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 2), Val is 50, !.
value(X,Y, C, Val):- contPawns(X, Y, C, 1), Val is 10, !.
value(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 1), Val is 5, !.
value(_,_, _, Val):- Val is 0, !.

%Update the value of a certain pawn/movement
update((_X, _Color), Value, (X1,Color1, Value1),(X1,Color1, Value1)):-
	Value < Value1.    
update((X, Color), Value, (_X1,_Color1, Value1),(X, Color, Value)):-
    Value > Value1.
%we add a randomizer in the case we have more options with equal values
update((X, Color), Value, (X1,Color1, Value1),(XO, ColorO, ValueO)):-
    Value = Value1, random_between(0,3,Nb), (Nb=0,  XO is X, ColorO = Color, ValueO is Value;
    									XO is X1, ColorO = Color1, ValueO is Value1).

%XMove is the column where the AI plays
%by increasing 2nd param(the depth) of evaluate_and_choose we increase difficulty and otherwise; 3 is the optimal value
choose_move(XMove, C) :- playableList(L), evaluate_and_choose(L, 3, 1, 0, (_X1, C, -100000), (XMove, _, _)),!.


evaluate_and_choose([], _Depth, _MaxMin, _ValueInit, (XRecord, Color, ValueRecord), (XRecord, Color, ValueRecord) ).
%Moves contains all available columns to insert
evaluate_and_choose([Move|Moves], Depth, MaxMin, ValueInit, (XRecord, Color, ValueRecord),(XBest, Color1, ValueBest) ):-
    (
    	add(Move, Color),
    	minimax(Depth, MaxMin, Move, Color, ValueInit, ValueReturned),
        %Color & NewColor (& Color1 ?) could be named the same, but I'm not sure, to try once it works well ^^
        update((Move,Color), ValueReturned, (XRecord, Color, ValueRecord), (XNewRecord, NewColor, ValueNewRecord) ),
        removePawn(Move), %remove the pawn added before starting exploring the next branch
        evaluate_and_choose(Moves, Depth, MaxMin, ValueInit, (XNewRecord, NewColor, ValueNewRecord), (XBest, Color1, ValueBest))
    ).

    
%Calculate the value of the considered move and add it to all the value of the moves already done
minimax(0, MaxMin, X, C, ValueInit, ValueReturn) :- height(X, Y), value(X, Y, C, V), ValueReturn is V*MaxMin + ValueInit.
minimax(D, MaxMin, X, C, ValueInit, ValueReturn) :- 
    D > 0,
    D1 is D-1,
    height(X, Y), 
    value(X, Y, C, V),
    ValueReturn is V*MaxMin + ValueInit,
    playableList(Moves),
    MinMax is -MaxMin,
    opposite(C, C1),
    evaluate_and_choose(Moves, D1, MinMax, ValueReturn, (_X, C1, -10000), (_X1, _C,_ValueReturn)).


%Check if the game is over and saying who won
end(X,Y,Color):-
    Color = yellow, win(X,Y,Color,3), write('Yellow (X) won!'), clear, !;
    Color = red, win(X,Y,Color,3), write('Red (O) won!'), clear, !;
    not(playable(_)), write('No more moves left :/'), clear, !.
