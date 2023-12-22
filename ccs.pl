:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').

tile(X, Tile) :- X = 1 , Tile = [c, c, p, c, 1].
tile(X, Tile) :- X = 2, Tile = [c, c, d, c, 1].
tile(X, Tile) :- X = 3, Tile = [c, c, p, p, 1].
tile(X, Tile) :- X = 4, Tile = [c, c, p, p, 2].
tile(X, Tile) :- X = 5, Tile = [c, p, c, p, 2].
tile(X, Tile) :- X = 6, Tile = [c, p, c, p, 1].
tile(X, Tile) :- X = 7, Tile = [c, p, p, p, 1].
tile(X, Tile) :- X = 8, Tile = [c, c, d, d, 1].
tile(X, Tile) :- X = 9, Tile = [c, p, d, d, 1].
tile(X, Tile) :- X = 10, Tile = [c, d, d, p, 1].  
tile(X, Tile) :- X = 11, Tile = [c, d, p, d, 1].
tile(X, Tile) :- X = 12, Tile = [c, d, d, d, 1].
tile(X, Tile) :- X = 13, Tile = [p, p, d, d, 0].
tile(X, Tile) :- X = 14, Tile = [p, d, p, d, 0].
tile(X, Tile) :- X = 15, Tile = [p, d, d, d, 0].
tile(X, Tile) :- X = 16, Tile = [d, d, d, d, 0].

at(T, X, W) :- X = n, T = [W | _ ].
at(T, X, W) :- X = e, T = [_, W | _].
at(T, X, W) :- X = s, T = [_, _, W | _].
at(T, X, W) :- X = w, T = [_, _, _, W | _].

atL(T, [X | []], W) :- at(T, X, W).
atL(T, [X | D], W) :- at(T, X, W), atL(T, D, W).

hasTwoCitadels(T) :- T = [_, _, _, _, W | _], W == 2.

ccw(T, 0, T).
ccw([N, E, S, W, Nr], R, Tr) :- R = 1, ccw([E, S, W, N, Nr], 0, Tr).
ccw([N, E, S, W, Nr], R, Tr) :- R = 2, ccw([S, W, N, E, Nr], 0, Tr).
ccw([N, E, S, W, Nr], R, Tr) :- R = 3, ccw([W, N, E, S, Nr], 0, Tr).

inlist([], _).
inlist([(_ , B) | L], E) :- B \== E, inlist(L, E).

 

rotatew(_, 4, Acc, Acc):-!.
rotatew(T, Nr, Acc, Rp) :- ccw(T, Nr, Rotation), inlist(Acc, Rotation), X = (Nr , Rotation), Nr1 is Nr + 1, rotatew(T, Nr1, [X | Acc], Rp),!.
rotatew(T, Nr, Acc, Rp) :- Nr1 is Nr + 1, rotatew(T, Nr1, Acc, Rp).
rotations(T, Rp) :- rotatew(T, 0, [], Rp).

match(T, N, D):- D = n, T1 = T, M1 = W1, at(T1, n, W1), N = N1, M2 = W2, at(N1, s, W2), M1 = M2,!.
match(T, N, D):- D = s, T1 = T, M1 = W1, at(T1, s, W1), N = N1, M2 = W2, at(N1, n, W2), M1 = M2,!.
match(T, N, D):- D = e, T1 = T, M1 = W1, at(T1, e, W1), N = N1, M2 = W2, at(N1, w, W2), M1 = M2,!.
match(T, N, D):- D = w, T1 = T, M1 = W1, at(T1, w, W1), N = N1, M2 = W2, at(N1, e, W2), M1 = M2.

findRotation(_, [], _):-!.
findRotation(T, [(X, Y) | L], R) :- T2 = T1, ccw(T, R, T1), match(T2, X, Y),
                                 findRotation(T, L, R).

emptyBoard([]).

boardSet([], P, T, [(P, T)]) :-!.
boardSet(B, P , T, O) :- canPlaceTile(B, P, T), O = [(P, T) | B].

boardGet([], _, _) :- false.
boardGet([(P1, T1) | L], P, T) :- P1 == P, T = T1;
                                boardGet(L, P, T).

boardmakelist([], Mi, Ma, Mi, Ma) :-!.
boardmakelist([((X, Y), _) | L], Acc1, Acc2, Mi, Ma) :- boardmakelist(L, [X | Acc1], [Y | Acc2], Mi, Ma). 

boardGetLimits([], _, _, _, _) :- false,!.
boardGetLimits(B, Xmi, Ymi, Xma, Yma) :- boardmakelist(B, [], [], Mi, Ma), min_list(Mi, Xmi), max_list(Mi, Xma), min_list(Ma, Ymi), max_list(Ma, Yma).

canPlaceTile([], _, _):-!.
canPlaceTile(B, (X , Y), T) :- \+ member(((X, Y), _), B), testn(B, (X, Y), T), teste(B, (X, Y), T), tests(B, (X, Y), T), testw(B, (X, Y), T), \+ testall(B, (X, Y), T).

testn(B, (X, Y), T) :- X1 is X - 1, \+ member(((X1, Y), _), B);
                       X1 is X - 1, match(T, T1, w), member(((X1, Y), _), B),  member(((X1, Y) , T1), B).
testw(B, (X, Y), T) :- Y1 is Y - 1, \+ member(((X, Y1), _), B);
                       Y1 is Y - 1, match(T, T1, s), member(((X, Y1), _), B),  member(((X, Y1) , T1), B).
tests(B, (X, Y), T) :- X1 is X + 1, \+ member(((X1, Y), _), B);
                       X1 is X + 1, match(T, T1, e), member(((X1, Y), _), B),  member(((X1, Y) , T1), B).
teste(B, (X, Y), T) :- Y1 is Y + 1, \+ member(((X, Y1), _), B);
                       Y1 is Y + 1, match(T, T1, n), member(((X, Y1), _), B),  member(((X, Y1) , T1), B).
testall(B, (X, Y), _) :- Y1 is Y + 1, X1 is X + 1, X2 is X - 1, Y2 is Y - 1,
                     \+ member(((X, Y1), _), B), \+ member(((X1, Y), _), B), \+ member(((X2, Y), _), B), \+ member(((X, Y2), _), B).

getAvaibleN(B, (X, Y), Ni, No) :- X1 is X, Y1 is Y + 1, \+member(((X1, Y1), _), B), \+member((X1, Y1), Ni), No = [(X1, Y1) | Ni],!.
getAvaibleN(_, (_, _), Ni, No) :- No = Ni.

getAvaibleS(B, (X, Y), Ni, No) :- X1 is X, Y1 is Y - 1, \+member(((X1, Y1), _), B), \+member((X1, Y1), Ni), No = [(X1, Y1) | Ni],!.
getAvaibleS(_, (_, _), Ni, No) :- No = Ni.

getAvaibleE(B, (X, Y), Ni, No) :- X1 is X + 1, Y1 is Y, \+member(((X1, Y1), _), B), \+member((X1, Y1), Ni), No = [(X1, Y1) | Ni],!.
getAvaibleE(_, (_, _), Ni, No) :- No = Ni.

getAvaibleV(B, (X, Y), Ni, No) :- X1 is X - 1, Y1 is Y, \+member(((X1, Y1), _), B), \+member((X1, Y1), Ni), No = [(X1, Y1) | Ni],!.
getAvaibleV(_, (_, _), Ni, No) :- No = Ni.

union(B, (X, Y), Ni, No) :- getAvaibleN(B, (X, Y), Ni, N1), getAvaibleE(B, (X, Y), N1, N2),
                            getAvaibleS(B, (X, Y), N2, N3), getAvaibleV(B, (X, Y), N3, No).

getanswer(_ ,[], P, P) :-!.
getanswer(L, [(P1, _) | R], Acc, P) :- union(L, P1, Acc, No), getanswer(L, R, No, P). 

getAvailablePositions([], _) :- false,!.
getAvailablePositions(L, P) :- L \== [], getanswer(L, L, [], P). 

ccw1(T, 0, Rez) :- ccw(T, 0, Rez).
ccw1(T, 1, Rez) :- tile(16, T1), T \== T1, ccw(T, 1, Rez).
ccw1(T, 3, Rez) :- tile(16, T0),tile(14, T1), tile(6, T2), tile(5, T3),T \== T0, T \== T1, T \== T2, T \== T3, ccw(T, 3, Rez).   
ccw1(T, 2, Rez) :- tile(16, T0),tile(14, T1), tile(6, T2), tile(5, T3),T \== T0, T \== T1, T \== T2, T \== T3, ccw(T, 2, Rez).
 

findPositionForTile([], _, (0,0), 0):-!. 
findPositionForTile(B, T, P, R) :- getAvailablePositions(B, A), member(P, A), ccw1(T, R, T1), canPlaceTile(B, P, T1).


append([], L, L).
append([H|T], L, [H|Res]) :- append(T, L, Res).

min(X, Y, M) :- X =< Y, M is X - 1.
min(X, Y, M) :- X > Y, M is Y - 1.

min2(X, Y, M) :- X =< Y, M = X - 1 .
min2(X, Y, M) :- X > Y, M = Y - 1.

filtersorted(Lin, Lout) :- findall([X], (member(X, Lin)), Lout).

even(Numbers, Even):-
    findall(X,
            (member(X, Numbers), X mod 2 =:= 0),
            Even).

