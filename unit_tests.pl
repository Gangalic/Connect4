%test height calculation
test_height:-add(2,red),add(2,red),add(2,red),height(2,Height),Height is 3, !.

%test simple add
test_add(Col,Line,Color):-add(Col,Line,Color),pawn(Col,Line,Color).

%test simple remove
test_remove(Col):-height(Col,Line),removePawn(Col),not(pawn(Col,Line-1,_)).

%test add and remove combination
test_add_remove:-test_add(3,1,red),test_remove(3),not(test_add(8,9,yellow)), !.
