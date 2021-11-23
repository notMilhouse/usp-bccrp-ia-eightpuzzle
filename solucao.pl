% operadores left, right, up e down

% mover a esquerda na linha superior 

move(left,[X1, 0,X3,X4,X5,X6,X7,X8,X9],[ 0,X1,X3,X4,X5,X6,X7,X8,X9]). 
move(left,[X1,X2,0, X4,X5,X6,X7,X8,X9],[X1, 0,X2,X4,X5,X6,X7,X8,X9]). 
% mover a esquerda na linha central 

move(left,[X1,X2,X3,X4, 0,X6,X7,X8,X9],[X1,X2,X3, 0,X4,X6,X7,X8,X9]). 
move(left,[X1,X2,X3,X4,X5, 0,X7,X8,X9],[X1,X2,X3,X4,0,X5,X7,X8,X9]). 
% mover a esquerda na linha inferior 

move(left,[X1,X2,X3,X4,X5,X6,X7, 0,X9],[X1,X2,X3,X4,X5,X6, 0,X7,X9]). 
move(left,[X1,X2,X3,X4,X5,X6,X7,X8, 0],[X1,X2,X3,X4,X5,X6,X7, 0,X8]). 
 

% mover a direita na linha superior 

move(right,[0,X2,X3,X4,X5,X6,X7,X8,X9],[X2, 0,X3,X4,X5,X6,X7,X8,X9]). 
move(right,[X1,0,X3,X4,X5,X6,X7,X8,X9],[X1,X3, 0,X4,X5,X6,X7,X8,X9]). 
% mover a direita na linha central 

move(right,[X1,X2,X3, 0,X5,X6,X7,X8,X9],[X1,X2,X3,X5, 0,X6,X7,X8,X9]). 
move(right,[X1,X2,X3,X4, 0,X6,X7,X8,X9],[X1,X2,X3,X4,X6, 0,X7,X8,X9]). 
% mover a direita na linha inferior 

move(right,[X1,X2,X3,X4,X5,X6, 0,X8,X9],[X1,X2,X3,X4,X5,X6,X8,0,X9]). 
move(right,[X1,X2,X3,X4,X5,X6,X7, 0,X9],[X1,X2,X3,X4,X5,X6,X7,X9,0]). 
 

% mover para cima a partir da linha central 

move(up,[X1,X2,X3, 0,X5,X6, X7,X8,X9], [0,X2,X3, X1,X5,X6, X7,X8,X9]). 
move(up,[X1,X2,X3, X4,0,X6, X7,X8,X9], [X1,0,X3, X4,X2,X6, X7,X8,X9]). 
move(up,[X1,X2,X3, X4,X5,0, X7,X8,X9], [X1,X2,0, X4,X5,X3, X7,X8,X9]). 
% mover para cima a partir da linha inferior 

move(up,[X1,X2,X3, X4,X5,X6, 0,X8,X9], [X1,X2,X3, 0,X5,X6, X4,X8,X9]). 
move(up,[X1,X2,X3, X4,X5,X6, X7,0,X9], [X1,X2,X3, X4,0,X6, X7,X5,X9]). 
move(up,[X1,X2,X3, X4,X5,X6, X7,X8,0], [X1,X2,X3, X4,X5,0, X7,X8,X6]). 
 

% mover para baixo a partir da linha superior 

move(down,[ 0,X2,X3, X4,X5,X6, X7,X8,X9],[X4,X2,X3,  0,X5,X6, X7,X8,X9]). 
move(down,[X1, 0,X3, X4,X5,X6, X7,X8,X9],[X1,X5,X3, X4, 0,X6, X7,X8,X9]). 
move(down,[X1,X2, 0, X4,X5,X6, X7,X8,X9],[X1,X2,X6, X4,X5, 0, X7,X8,X9]). 
% mover para baixo a partir da linha central 

move(down,[X1,X2,X3,  0,X5,X6, X7,X8,X9],[X1,X2,X3, X7,X5,X6,  0,X8,X9]). 
move(down,[X1,X2,X3, X4, 0,X6, X7,X8,X9],[X1,X2,X3, X4,X8,X6, X7, 0,X9]). 
move(down,[X1,X2,X3, X4,X5, 0, X7,X8,X9],[X1,X2,X3, X4,X5,X9, X7,X8, 0]).

position_coordinates(a, 1, 1).
position_coordinates(b, 2, 1).
position_coordinates(c, 3, 1).

position_coordinates(d, 1, 2).
position_coordinates(e, 2, 2).
position_coordinates(f, 3, 2).

position_coordinates(g, 1, 3).
position_coordinates(h, 2, 3).
position_coordinates(i, 3, 3).

distance_between_positions(A, B, DISTANCE) :-
    position_coordinates(A, XA, YA),
    position_coordinates(B, XB, YB),
    DISTANCE is (abs(XA - XB) + abs(YA - YB)).

% distance null or not null (so counts as out of position)
distance_null(0, 0).
distance_null(X, 1) :-
    \+ X = 0.

goal_state([1,2,3,8,0,4,7,6,5]).

% a b c
% d e f 
% g h i

% 1 2 3
% 8 _ 4 
% 7 6 5

piece_goal_position(1, a).
piece_goal_position(2, b).
piece_goal_position(3, c).
piece_goal_position(4, f).
piece_goal_position(5, i).
piece_goal_position(6, h).
piece_goal_position(7, g).
piece_goal_position(8, d).
piece_goal_position(0, e).

piece_distance_to_goal(PIECE, POSITION, DISTANCE_TO_GOAL) :-
    piece_goal_position(PIECE, GOAL_POSITION),
    distance_between_positions(POSITION, GOAL_POSITION, DISTANCE_TO_GOAL).

state_elements_positions([X1,X2,X3,X4,X5,X6,X7,X8,X9], [a,b,c,d,e,f,g,h,i]).

% distance of all pieces to their goal positions
manhattan_distance([], [], 0, 0).

manhattan_distance([X0 | OtherElements], [PosX0 | OtherPositions], H, G) :-
    piece_distance_to_goal(X0, PosX0, H0),
    distance_null(H0, G0),
    manhattan_distance(OtherElements, OtherPositions, H1, G1),
    H is H0 + H1,
    G is G0 + G1.

h_function(EstadoAtual, F) :-
    state_elements_positions(EstadoAtual, PosicoesElementos),
    manhattan_distance(EstadoAtual, PosicoesElementos, H, G),
    F is H + G.




:- op(400,yfx,'#').    /* Node builder notation */

solve(State,Soln) :- f_function(State,0,F),
                     search([State#0#F#[]],S), reverse(S,Soln), !.

f_function(State,D,F) :- h_function(State,H),
                         F is D + H.

search([State#_#_#Soln|_], Soln) :- goal_state(State).
search([B|R],S) :- expand(B,Children),
                   insert_all(Children,R,Open),
                   search(Open,S).

insert_all([F|R],Open1,Open3) :- insert(F,Open1,Open2),
                                 insert_all(R,Open2,Open3).
insert_all([],Open,Open).

insert(B,Open,Open) :- repeat_node(B,Open), ! .
insert(B,[C|R],[B,C|R]) :- cheaper(B,C), ! .
insert(B,[B1|R],[B1|S]) :- insert(B,R,S), !.
insert(B,[],[B]).

repeat_node(P#_#_#_, [P#_#_#_|_]).

cheaper( _#_#F1#_ , _#_#F2#_ ) :- F1 < F2.

expand(State#D#_#S,All_My_Children) :-
     bagof(Child#D1#F#[Move|S],
           (D1 is D+1,
             move(Move, State, Child),
             f_function(Child,D1,F)),
           All_My_Children).