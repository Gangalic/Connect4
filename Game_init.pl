%Creating pawn with 3 args allowing to assert predicates correctly
:- dynamic pawn/3.

%clear the "database"
clear :- retractall(pawn(_X,_Y,_Z)).

%displaying each pawn or its absence
display(X, Y) :- pawn(X, Y, red), write('o|').
display(X, Y) :- pawn(X, Y, yellow), write('x|').
display(_, _) :- write(' .|').

%displaying the virtual playing table
display :-
    between(1, 6, Tmp), Y is 7-Tmp, nl, write('|'),
    between(1, 7, X), not(display(X, Y)) ; true, !.

%add pawn with specified height
add(X, Y, Color) :- 
    integer(X), X &gt;= 1, X &lt; 8,
    height(X, Count), Y is Count+1,
    integer(Y), Y &gt;= 1, Y &lt; 7,
    asserta(pawn(X, Y, Color)).

%add pawn at good height in column X
add(X, Color) :-
    height(X, Count), Y is Count+1,
    add(X, Y, Color).

play(X) :-
    (add(X, yellow), ! ; write('You can not play there !'), nl, abort), display, nl.     %If pawn was added we display, else we abort
   % (end(X, Y, rouge), ! ; play).       %Then, if we dont win then IA have to make a move (play)

%calculate acceptable height
height(X, Count) :- aggregate_all(count, pawn(X, _, _), Count).

%calculating how many pawns of same color are there
%in the vector(not line, as it has direction) defined by (X,Y)-&gt;(DeltaX,DeltaY)
%and giving the count number as result
around(X, Y, DeltaX, DeltaY, Color, Count) :- 
    (
        between(1, 3, Counter), 
        NextX is DeltaX*Counter+X, NextY is DeltaY*Counter+Y,
        %check if pawn of not this color (or inexistent) and decreasing the final count value
        not(pawn(NextX, NextY, Color)),
        Count is Counter-1, !
    ) ; Count is 3, true.

%checking if any winning combination after possible adding of (X,Y)
win(X, Y, C) :- around(X, Y, 1, 0, C, C1), around(X, Y, -1, 0, C, C2), Count is C1+C2, Count &gt;= 3, !. %horizontal
win(X, Y, C) :- around(X, Y, 0, 1, C, C1), around(X, Y, 0, -1, C, C2), Count is C1+C2, Count &gt;= 3, !. %vertical
win(X, Y, C) :- around(X, Y, 1, -1, C, C1), around(X, Y, -1, 1, C, C2), Count is C1+C2, Count &gt;= 3, !. %SE-&gt;NW diag
win(X, Y, C) :- around(X, Y, 1, 1, C, C1), around(X, Y, -1, -1, C, C2), Count is C1+C2, Count &gt;= 3, !. %SW-&gt;NE diag
