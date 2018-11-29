%Creating pawn with 3 args allowing to assert predicates correctly
:- dynamic pawn/3.

%clear the "database"
clear :- retractall(pawn(_X,_Y,_Z)).

%remove a certain pawn
removePawn(X) :- height(X,Height),retract(pawn(X,Height,_)).
removePawn(X,Y) :- retract(pawn(X,Y,_)).

%gives column numbers where we can still insert
playable(X) :- between(1, 7, X), once(not(pawn(X, 6, _))).

%gives column numbers where we can still insert into a list
playableList(L) :- findall(X, playable(X), L).
    
%Simples rules of displaying
display(X, Y) :- pawn(X, Y, red), write('o|').
display(X, Y) :- pawn(X, Y, yellow), write('x|').
display(_, _) :- write(' .|').

%Displaying the grid
%Can use between because display is always true so always not false ;)
display :-
    between(1, 6, Tmp), Y is 7-Tmp, nl, write('|'),
    between(1, 7, X), not(display(X, Y)) ; true, !.

%add pawn with specified height
add(X, Y, Color) :- 
    integer(X), X >= 1, X < 8,
    height(X, Count), Y is Count+1,
    integer(Y), Y >= 1, Y < 7,
    asserta(pawn(X, Y, Color)).

%add pawn at good height in column X
add(X, Color) :-
    add(X, _, Color).

play(X) :-
    (add(X, Y, yellow), ! ; write('You can not play there !'), nl, abort), display, nl,     %If pawn was added we display, else we abort
    (end(X, Y, red), ! ; playIA).       %Then, if we dont win then IA have to make a move (play)
    
playTwoPlayers(Color):-
    (getX(X), add(X, Y, Color), ! ; write('You cannot play there! Try again'), nl, playTwoPlayers(Color)), display, nl,     %If pawn was added we display, else we abort
    (end(X, Y, Color), ! ; opposite(Color, NextColor), nl, write('---Player change---'), nl, playTwoPlayers(NextColor)).

getX(X):-
    write('Choose column to insert:'), read(X).

ia(X, Y, Color) :- X is random(X), add(X, Y, Color), ! .
%If the first rules is false (full column), the ia plays again until finding a solution  :
ia(X, Y, Color) :- ia(X,Y, Color), !.

playIA :- ia(X,Y,Color), nl, write('AI turn:'), display, nl, end(X,Y,Color).
    


%calculate acceptable height
height(X, Count) :- aggregate_all(count, pawn(X, _, _), Count).

%calculating how many pawns of same color are there
%in the vector(not line, as it has direction) defined by (X,Y)->(DeltaX,DeltaY)
%and giving the count number as result
around(X, Y, DeltaX, DeltaY, Color, Count) :- 
    (
        between(1, 3, Counter), 
        NextX is DeltaX*Counter+X, NextY is DeltaY*Counter+Y,
        %check if pawn of not this color (or inexistent) and decreasing the final count value
        not(pawn(NextX, NextY, Color)),
        Count is Counter-1, !
    ) ; Count is 3, true.

%is true if there is a serie of Lim pawns of the same color C alligned in any specific direction (without X Y pawn)
contPawns(X, Y, C, Lim) :- around(X, Y, 1, 0, C, C1), around(X, Y, -1, 0, C, C2), Count is C1+C2, Count >= Lim, !. %horizontal
contPawns(X, Y, C, Lim) :- around(X, Y, 0, 1, C, C1), around(X, Y, 0, -1, C, C2), Count is C1+C2, Count >= Lim, !. %vertical
contPawns(X, Y, C, Lim) :- around(X, Y, 1, -1, C, C1), around(X, Y, -1, 1, C, C2), Count is C1+C2, Count >= Lim, !. %SE->NW diag
contPawns(X, Y, C, Lim) :- around(X, Y, 1, 1, C, C1), around(X, Y, -1, -1, C, C2), Count is C1+C2, Count >= Lim, !. %SW->NE diag

%checking if any winning combination after possible adding of (X,Y)
win(X, Y, C, 3) :-  contPawns(X, Y, C, 3).

%change color
opposite(Color, ColorOpp) :- Color = yellow, ColorOpp = red, !.
opposite(Color, ColorOpp) :- Color = red, ColorOpp = yellow, !.

%we consider that the pawn is already added, and the withdraw is done after
value(X,Y, C, Val):- contPawns(X, Y, C, 3), Val is 1000, !.
value(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 3), Val is 500, !.
value(X,Y, C, Val):- contPawns(X, Y, C, 2), Val is 100, !.
value(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 2), Val is 50, !.
value(X,Y, C, Val):- contPawns(X, Y, C, 1), Val is 10, !.
value(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 1), Val is 5, !.
value(_,_, _, Val):- Val is 0, !.


update((_X, _Color), Value, (X1,Color1, Value1),(X1,Color1, Value1)):-
	Value =< Value1.    
update((X, Color), Value, (_X1,_Color1, Value1),(X, Color, Value)):-
    Value > Value1.

%XMove is the column where the ia plays
choose_move(XMove, C) :- playableList(L), evaluate_and_choose(L, 3, 1, 0, (_X1, C, -100000), (XMove, _, _)).
    %setof(XMove, playableList(L), L), evaluate_and_choose(L, 3, 1, 0, (_X1, C, -100000), (XMove, _, _)).



evaluate_and_choose([], _Depth, _MaxMin, _ValueInit, (XRecord, Color, ValueRecord), (XRecord, Color, ValueRecord) ).

%%Moves contains the X available
evaluate_and_choose([Move|Moves], Depth, MaxMin, ValueInit, (XRecord, Color, ValueRecord),(XBest, Color1, ValueBest) ):-
    (
    	add(Move, Color),
    	minimax(Depth, MaxMin, Move, Color, ValueInit, ValueReturned),
        %Color & NewColor (& Color1 ?) could be named the same, but I'm not sure, to try once it works well ^^
        update((Move,Color), ValueReturned, (XRecord, Color, ValueRecord), (XNewRecord, NewColor, ValueNewRecord) ),
        removePawn(Move), %remove the pawn added before starting exploring the next branch
        evaluate_and_choose(Moves, Depth, MaxMin, ValueInit, (XNewRecord, NewColor, ValueNewRecord), (XBest, Color1, ValueBest))
    ).


    
%calculates the value of the considered move, adds it to all the value of the moves already done, 
minimax(0, MaxMin, X, C, ValueInit, ValueReturn) :- height(X, Y), value(X, Y, C, V), ValueReturn is V*MaxMin + ValueInit.
minimax(D, MaxMin, X, C, ValueInit, ValueReturn) :- 
    D > 0, %this line might be souce of bugs (return false ?)
    D1 is D-1,
    height(X, Y), 
    value(X, Y, C, V),
    ValueReturn is V*MaxMin + ValueInit,
    playableList(Moves),
    MinMax is -MaxMin,
    opposite(C, C1),
    evaluate_and_choose(Moves, D1, MinMax, ValueReturn, (_X, C1, -10000), (_X1, _C,_ValueReturn)).

%play:- (between(1,4,_),
 %   write('Select column'),
  %  read(X),
   % catch(play(X),_E, write('next'))); true.

end(X,Y,Color):-
    Color = yellow, win(X,Y,Color,3), write('You won!'), clear, !;
    Color = red, win(X,Y,Color,3), write('You lost!'), clear, !;
    not(playable(_)), write('No more moves left :/'), clear, !.
