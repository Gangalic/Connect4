:- use_module(game).

%test random player against normal AI
end_with_winner(X,Y,Color,WinnerColor):-
    Color = yellow, win(X,Y,Color,3), write('Random won!'), nl, clear, WinnerColor = Color, !;
    Color = red, win(X,Y,Color,3), write('AI won!'), nl, clear, WinnerColor = Color, !;
    not(playable(_)), write('No more moves left :/'), nl, clear, WinnerColor = green, !.

%make AI play versus random insert
playRandomVersusAI(WinnerColor):-
    random(1,7,X), add(X, Y, yellow), 
    ( end_with_winner(X, Y, yellow,WinnerColor), ! ; choose_move(Xchosen, red), add(Xchosen, Ychosen, red), 
        (end_with_winner(Xchosen, Ychosen, red,WinnerColor), ! ; playRandomVersusAI(WinnerColor)) ).

%test 100 cases of games against random
test_AI_efficiency:-
	between(1,100,_),
	playRandomVersusAI(_).
