append([],L,L).
append([H|T],L,[H|R]):- append(T,L,R).
makelist1(First, Rest, List):- List = [First |Rest].
makelist2(First, Rest, [First |Rest]).
memb1(Item, List) :-append(_,[Item | _], List).
del( X, [X|Tail], Tail).
del(X, [Y |Tail], [Y |Tail1]) :- del(X, Tail, Tail1).
memb2(Item, List) :- del(Item, List, _).
%sublist(S, L) :- app(L1, L2, L), app(S, L3, L2).
%Task 1
prime(1).
prime(2).
prime(X):-Y is floor(sqrt(X)),
	isprime(X, Y).
isprime(_, 1).
isprime(X, Y) :-
	(X mod Y) > 0,
	Y1 is Y-1,
	isprime(X, Y1).

%task 2
primesto(X, L):-write('beg'),nl,build_p_list(X, X, L).
build_p_list(_, 0, []).
build_p_list(X, Y, [Y|L]):- Y > 0,
	prime(Y),
	Y1 is Y-1,
	build_p_list(X, Y1, L).
build_p_list(X, Y, L):- Y>0,
	not(prime(Y)),
	Y1 is Y-1,
        build_p_list(X, Y1, L).

%Task 3 Hermione's logic test
%
%4th- same kind 2nd left and 2nd right, different size X
%1st-poison left of wine
%2nd -ahead not at ends, different on either side X
%3rd- all different size, smallest, largest have no poison X
poisonToLeft(_,6).
poisonToLeft(Bottles, Index):-
	nth0(Index, Bottles, W),
	W = wine,
	Left is Index -1,
	nth0(Left, Bottles, P),
	P = poison,
	I2 is Index + 1,
	poisonToLeft(Bottles, I2).
poisonToLeft(Bottles, Index):-
	nth0(Index, Bottles, B),
	not(B = wine),
	I2 is Index + 1,
	poisonToLeft(Bottles, I2).
herm(Bottles, Smallest, Largest):-
	permutation(Bottles, [poison, wine, ahead, poison, poison, wine, back]),	Bottles = [B1, B2, _, _, _, B6, B7],
	%rule #3
	nth1(Smallest, Bottles, S),
	nth1(Largest, Bottles, L),
	not(L = poison),
	not(S = poison),
	%rule # 1
	/*W1 = wine,
	W2 = wine,
	nth0(W1P, Bottles, W1),
	nth0(W2P, Bottles, W2),
	nth0(P1P, Bottles, P1),
	nth0(P2P, Bottles, P2),
	P1 = poison,
	P2 = poison,
        W1P is P1P + 1,
	W2P is P2P + 1,*/
	poisonToLeft(Bottles, 1),
	%rule #4
	B2 = B6,
	%rule #2
	not(B1 = B7),
	not(B1 = ahead),
	not(B7 = ahead).
pairdiffelements(Set, Pairs):-goAll(0, 0, Set, Pairs).
goAll(I, length(Set), Set, Pairs):-I2 is I+1,
	J2 is I2+1,
	goAll(I2, J2, Set, Pairs).
goAll(I, J, Set, [NewPair|Pairs]):-
	nth0(I, Set, EltI),
	nth0(J, Set, EltJ),
	NewPair = [EltI, EltJ],
	J2 is J+1,
	goAll(I, J2, Set, [NewPair|Pairs]).
%solver(Bottles, Smallest, Largest):-
%Task 4 A water jug









