:- module(defensive_IA, [choose_move_defensive_IA/2]).

:- use_module(game).

%XMove is the column where the AI plays
%by increasing 2nd param(the depth) of evaluate_and_choose we increase difficulty and otherwise; 3 is the optimal value
choose_move_defensive_IA(XMove, C) :- playableList(L), evaluate_and_choose_defensive(L, 3, 1, 0, (_X1, C, -100000), (XMove, _, _)),!.


evaluate_and_choose_defensive([], _Depth, _MaxMin, _ValueInit, (XRecord, Color, ValueRecord), (XRecord, Color, ValueRecord) ).
%Moves contains all available columns to insert
evaluate_and_choose_defensive([Move|Moves], Depth, MaxMin, ValueInit, (XRecord, Color, ValueRecord),(XBest, Color1, ValueBest) ):-
    (
        add(Move, Color),
        minimax_defensive(Depth, MaxMin, Move, Color, ValueInit, ValueReturned),
        %Color & NewColor (& Color1 ?) could be named the same, but I'm not sure, to try once it works well ^^
        update((Move,Color), ValueReturned, (XRecord, Color, ValueRecord), (XNewRecord, NewColor, ValueNewRecord) ),
        removePawn(Move), %remove the pawn added before starting exploring the next branch
        evaluate_and_choose_defensive(Moves, Depth, MaxMin, ValueInit, (XNewRecord, NewColor, ValueNewRecord), (XBest, Color1, ValueBest))
    ).


%Calculate the value of the considered move and add it to all the value of the moves already done
minimax_defensive(0, MaxMin, X, C, ValueInit, ValueReturn) :- height(X, Y), valueDefensive(X, Y, C, V), ValueReturn is V*MaxMin + ValueInit.
minimax_defensive(D, MaxMin, X, C, ValueInit, ValueReturn) :- 
    D > 0,
    D1 is D-1,
    height(X, Y), 
    valueDefensive(X, Y, C, V),
    ValueReturn is V*MaxMin + ValueInit,
    playableList(Moves),
    MinMax is -MaxMin,
    opposite(C, C1),
    evaluate_and_choose_defensive(Moves, D1, MinMax, ValueReturn, (_X, C1, -10000), (_X1, _C,_ValueReturn)).

%Calculate the value(power) of putting a certain pawn here
%we consider that (X,Y) pawn is already added, and the withdraw is done after
valueDefensive(X,Y, C, Val):- contPawns(X, Y, C, 3), Val is 1000, !.
valueDefensive(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 3), Val is 500, !.
valueDefensive(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 2), Val is 50, !.
valueDefensive(X,Y, C, Val):- opposite(C, C1), contPawns(X, Y, C1, 1), Val is 5, !.
valueDefensive(_,_, _, Val):- Val is 0, !.