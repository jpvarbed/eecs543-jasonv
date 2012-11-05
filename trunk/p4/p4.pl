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
poisonToLeft(_,7).
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
	poisonToLeft(Bottles, 1),
	%rule #4
	B2 = B6,
	%rule #2
	not(B1 = B7),
	not(B1 = ahead),
	not(B7 = ahead).
%pairdiffelements(Set, Pairs):-goAll(0, 0, Set, Pairs).
pairdiffelements(Set,Pairs):-matchAll(Set, Set, Pairs).

matchAll([E|_], All, [L1, L2|[]]):-
	L1 = E,
	matchSecond(L1, L2, All).

matchAll([_|Set], All, Pairs):-
	matchAll(Set, All, Pairs).

matchSecond(L1, L2, [E|_]):-
	match(L1, L2, E).
matchSecond(L1, L2, [_|Elts]):-
	matchSecond(L1, L2, Elts).
match(L1, L2, E):-
	not(L1 = E),
	L2 = E.

goAll(0, 0, Set, _):-
	goAll(1, 2, Set, []).
goAll(L, _, Set, Pairs):-proper_length(Set, L),
	proper_length(Pairs, L2),
	L2 > 0.
goAll(I, X, Set, Pairs):-
	write('X is '), write(X),
	proper_length(Set, L),
	X = L + 1,
	write('next loop'),
	I2 is I+1,
	J2 is I2+1,
	write(' I2 '), write(I2), write(' J2 '), write(J2), nl,
	goAll(I2, J2, Set, Pairs).
goAll(I, J, Set, Pairs):-
	proper_length(Set, L),
	J < L + 1,
	nth1(I, Set, EltI),
	nth1(J, Set, EltJ),
	NewPair = [EltI, EltJ],
	member(NewPair, Pairs),
	J2 is J+1,
	write('J2 is '), write(J2), write(' Pairs is '), write(Pairs), nl,
	goAll(I, J2, Set, Pairs).
%Task 4 A water jug
%T is three, F is five, E is eight
%3 bottle

oper([T, F, E], [NT, F, NE], Action):-
	Left is 3 - T, Left >= E, Left > 0, E > 0,
	NT is T + E, NE = 0,
	Action = 'Empty 8-quart into 3-quart'.
oper([T, F, E], [NT, F, NE], Action):-
	Left is 3 - T, Left < E, Left > 0, E > 0,
	NT is 3, NE is E - Left,
	Action = 'Fill 3-quart from 8-quart'.
oper([T, F, E], [NT, NF, E], Action):-
	Left is 3 - T, Left >= F, Left > 0, F > 0,
	NT is T + F, NF = 0,
	Action = 'Empty 5-quart into 3-quart'.
oper([T, F, E], [NT, NF, E], Action):-
	Left is 3 - T, Left < F, Left > 0, F > 0,
	NT = 3, NF is F - Left,
	Action = 'Fill 3-quart from 5-quart'.

%into 5 bottle
oper([T,F, E], [T, NF, NE], Action):-
	Left is 5 - F, Left >= E, Left > 0, E > 0,
	NE is 0, NF is F + E,
	Action = 'Empty 8-quart into 5-quart'.
oper([T, F, E], [T, NF,NE], Action):-
	Left is 5 - F, Left < E, Left > 0, E > 0,
	NF = 5, NE is E - Left,
	Action = 'Fill 5-quart from 8-quart'.
oper([T, F, E], [NT, NF, E], Action):-
	Left is 5 - F, Left >= T, Left > 0, T > 0,
	NT = 0,	NF is T + F,
	Action = 'Empty 3-quart into 5-quart'.
oper([T, F, E], [NT, NF, E], Action):-
	Left is 5 - F, Left < T, Left > 0, T > 0,
	NF = 5, NT is 3 - Left,
	Action = 'Fill 5-quart from 3-quart'.

%into 8
oper([T, F, E], [T, NF, NE], Action):-
	Left is 8 - E, Left < F, Left > 0, F > 0,
	NE = 8, NF is F - Left,
	Action = 'Fill 8-quart from 5-quart'.
oper([T,F, E], [T, NF, NE], Action):-
	Left is 8 - E, Left >= F, Left > 0, F > 0,
	NE is F + E, NF = 0,
	Action = 'Empty 5-quart into 8-quart'.
oper([T, F, E], [NT, F, NE], Action):-
	Left is 8 - E, Left >= T, Left > 0, T > 0,
	NE is E + T, NT = 0,
	Action = 'Empty 3-quart into 8-quart'.
oper([T, F, E], [NT, F, NE], Action):-
	Left is 8 - E, Left < T, Left > 0, T > 0,
	NT is T - Left, NE = 8,
	Action = 'Fill 8-quart from 3-quart'.

printPath([]).
printPath([First|Rest]):- write(First),	nl, printPath(Rest).
waterjug(Start, Goal):- solve(Start, [], [], Goal).

solve(Goal, Path, _, Goal):- write('Solution is'), nl,
	reverse(Path, RPath),
	printPath(RPath).

solve(State, Path, GStack, Goal):-
	not(State = Goal),
	oper(State, NewState, Action),
	not(member(NewState,GStack)),
	solve(NewState, [Action|Path], [NewState|GStack], Goal).




















