:- use_module(game).

%test if pawn is correctly added
test_height:-add(2,red),add(2,red),add(2,red),height(2,Height),Height is 3, !.
test_add(Col,Line,Color):-add(Col,Line,Color),pawn(Col,Line,Color).
test_remove(Col):-height(Col,Line),removePawn(Col),not(pawn(Col,Line-1,_)).
test_add_remove:-test_add(3,1,red),test_remove(3),not(test_add(8,9,yellow)), !.

%test random player against normal AI
end_with_winner(X,Y,Color,WinnerColor):-
    Color = yellow, win(X,Y,Color,3), write('Random won!'), nl, clear, WinnerColor = Color, !;
    Color = red, win(X,Y,Color,3), write('AI won!'), nl, clear, WinnerColor = Color, !;
    not(playable(_)), write('No more moves left :/'), nl, clear, WinnerColor = green, !.

playRandomVersusAI(WinnerColor):-
    random(1,7,X), add(X, Y, yellow), 
    ( end_with_winner(X, Y, yellow,WinnerColor), ! ; choose_move(Xchosen, red), add(Xchosen, Ychosen, red), 
        (end_with_winner(Xchosen, Ychosen, red,WinnerColor), ! ; playRandomVersusAI(WinnerColor)) ).
	
test_AI_efficiency:-
	between(1,100,_),
	playRandomVersusAI(_).
