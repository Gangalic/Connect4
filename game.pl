:- module(game, [removePawn/1, playableList/1, add/2, height/2, opposite/2, contPawns/4, update/4]).

:- use_module(normal_IA).
:- use_module(offensive_IA).
:- use_module(defensive_IA).

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
    ( end(X, Y, yellow), ! ; nl, write('AI playing...'), choose_move_normal_IA(Xchosen, red), add(Xchosen, Ychosen, red), display, nl, 
        (end(Xchosen, Ychosen, red), ! ; playVersusAI) ).       %Then, if we don't win AI has to make a move (play)
    
%Start normal game against random AI
playVersusAIRandom:-
    (getX(X), write('X:'), add(X, Y, yellow), ! ; write('You cannot play there! Please, try again.'), nl, playVersusAI), display, nl,     %If pawn was added we display, else we retry
    ( end(X, Y, yellow), ! ; nl, write('AI playing...'), random_between(1, 7, Xchosen), add(Xchosen, Ychosen, red), display, nl, 
        (end(Xchosen, Ychosen, red), ! ; playVersusAIRandom) ).       %Then, if we don't win AI has to make a move (play)

%Start two AIs playing against each other
playTwoAIs(Color):-
    ( nl, write('AI '), write(Color), write(' playing...'), choose_move_offensive_IA(Xchosen, Color), add(Xchosen, Ychosen, Color), display, nl, 
        (end(Xchosen, Ychosen, Color), ! ; opposite(Color, NextColor), playTwoAIs(NextColor)) ).

%Start game between 2 physical players
playTwoPlayers(Color):-
    (getX(X), add(X, Y, Color), ! ; write('You cannot play there! Please, try again.'), nl, playTwoPlayers(Color)), display, nl,     %If pawn was added we display, else we abort
    (end(X, Y, Color), ! ; opposite(Color, NextColor), nl, write('---Player change---'), nl, playTwoPlayers(NextColor)).

%Gives true if there is a serie of at least Lim pawns of the same color C alligned in any specific direction (without (X,Y) pawn)
contPawns(X, Y, C, Lim) :- around(X, Y, 1, 0, C, C1), around(X, Y, -1, 0, C, C2), Count is C1+C2, Count >= Lim, !. %horizontal
contPawns(X, Y, C, Lim) :- around(X, Y, 0, 1, C, C1), around(X, Y, 0, -1, C, C2), Count is C1+C2, Count >= Lim, !. %vertical
contPawns(X, Y, C, Lim) :- around(X, Y, 1, -1, C, C1), around(X, Y, -1, 1, C, C2), Count is C1+C2, Count >= Lim, !. %SE->NW diag
contPawns(X, Y, C, Lim) :- around(X, Y, 1, 1, C, C1), around(X, Y, -1, -1, C, C2), Count is C1+C2, Count >= Lim, !. %SW->NE diag


%Change color to its opposite (red to yellow; yellow to red)
opposite(Color, ColorOpp) :- Color = yellow, ColorOpp = red, !.
opposite(Color, ColorOpp) :- Color = red, ColorOpp = yellow, !.


%Update the value of a certain pawn/movement
update((_X, _Color), Value, (X1,Color1, Value1),(X1,Color1, Value1)):-
    Value < Value1.    
update((X, Color), Value, (_X1,_Color1, Value1),(X, Color, Value)):-
    Value > Value1.
%we add a randomizer in the case we have more options with equal values
update((X, Color), Value, (X1,Color1, Value1),(XO, ColorO, ValueO)):-
    Value = Value1, random_between(0,3,Nb), (Nb=0,  XO is X, ColorO = Color, ValueO is Value;
                                        XO is X1, ColorO = Color1, ValueO is Value1).


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


%Check if any winning combination after possible adding of (X,Y)
win(X, Y, C, 3) :-  contPawns(X, Y, C, 3).
    

%Check if the game is over and saying who won
end(X,Y,Color):-
    Color = yellow, win(X,Y,Color,3), write('Yellow (X) won!'), clear, !;
    Color = red, win(X,Y,Color,3), write('Red (O) won!'), clear, !;
    not(playable(_)), write('No more moves left :/'), clear, !.