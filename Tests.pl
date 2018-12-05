:- use_module(game).


%test if pawn is correctly added
%
test_height:-add(2,red),add(2,red),add(2,red),height(2,Height),Height is 3.
test_add(Col,Line,Color):-add(Col,Line,Color),pawn(Col,Line,Color).
test_remove(Col):-height(Col,Line),removePawn(Col),not(pawn(Col,Line-1,_)).
test_add_remove:-test_add(3,1,red),test_remove(3),not(test_add(8,9,yellow)).

%test random player against normal AI
%
end_with_winner(X,Y,Color,WinnerColor):-
    Color = yellow, win(X,Y,Color,3), write('Random won!'), clear, WinnerColor is Color, !;
    Color = red, win(X,Y,Color,3), write('AI won!'), clear, WinnerColor is Color, !;
    not(playable(_)), write('No more moves left :/'), clear, WinnerColor = green, !.

playRandomVersusAI(WinnerColor):-
    random(1,7,X), height(X,Y), add(X, Y, yellow), display, nl,     %If pawn was added we display, else we retry
    ( end_with_winner(X, Y, yellow,WinnerColor), ! ; nl, write('AI playing...'), choose_move(Xchosen, red), add(Xchosen, Ychosen, red), display, nl, 
        (end_with_winner(Xchosen, Ychosen, red,WinnerColor), ! ; playRandomVersusAI(WinnerColor)) ).       %Then, if we don't win AI has to make a move (play)
    
	
%test_AI_efficiency(RandomVictories,AIVictories,NullMatches):-
	%between(1,20,I)( 
	%	playRandomVersusAI(WinnerColor),
	%	(WinnerColor = yellow, RandomVictories1 is RandomVictories, incr(RandomVictories1,RandomVictories),!;
	%	WinnerColor = red, AIVictories1 is AIVictories, incr(AIVictories1,AIVictories),!;
	%	WinnerColor = green, NullMatches1 is NullMatches, incr(NullMatches1,NullMatches),!)
	%).%%
	
	
incr(X, X1):-X1 is X+1.
