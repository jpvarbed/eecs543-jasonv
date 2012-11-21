% Student Name:  Jason Varbedian, Dan Jonik
% 244 in pdf


% Task 4
decide(example(Class, List), tree(Main, Subtrees)):-
	%match first with first, holes to holes
	%find one that works from that tree
	write(Class), nl, write(List), nl,
	write(Main), nl,write(Subtrees),
	findTop(Main, List, Val),
	nl, write('Answer'), nl,
	write(Val),
	getTree(Val, Subtrees, tree(Att, AttList)),
	nl, write('Att in tree'), write(Att), write(' to match '), write(AttList), nl,
	getAttrVal(AV, Att, List),
	write('AV is '), write(AV), nl,
	match(AV, AttList, Class).
findTop(Main, [Main=Val|_], Val).
findTop(Main, [_|Rest], Val):- findTop(Main, Rest, Val).
getTree(Val, [Val:Tree|_], Tree).
getTree(Val, [_|Rest], Tree):-getTree(Val, Rest, Tree).
getAttrVal(AttrVal, Att, [Att=AttrVal|_]).
getAttrVal(AttrVal, Att, [_|Rest]):-getAttrVal(AttrVal, Att, Rest).
match(AttrVal, [_|Rest], Class):-match(AttrVal, Rest, Class).
match(AttrVal, [AttrVal:leaf(Class)|_], Class).



%Task 2
parseList([]).

parseList([Val:null], Indent) :-
	tab(Indent),write(Val),write(' ==> null'), nl.

parseList([Val:leaf(Name)], Indent) :-
    tab(Indent), 
    write(Val), write(' ==> '), write(Name), nl.

%what if not leaf? idk.
parseList([Val:leaf(Name)|Rest], Indent) :-
	tab(Indent),
	write(Val), write(' ==> '), write(Name), nl,
	parseList(Rest, Indent).


parseTree(Number:null, Indent) :-
	tab(Indent), write(Number), write(' ==> null'), nl.

parseTree(Val:tree(Att, List), Indent) :-
	tab(Indent),
	write(Val), nl,
    Indent2 is Indent + 3,
	tab(Indent2), write(Att), nl,
    Indent3 is Indent2 + 3,
	parseList(List, Indent3).


parseBigList([], _Indent).

parseBigList(List, Indent) :-
    parseList(List, Indent).

parseBigList([First|Rest], Indent) :-
	%write('big'),nl, write(First),nl, write('rest'),nl, write(Rest),
	parseTree(First, Indent),
	parseBigList(Rest, Indent).


show(tree(Att, Rest)) :-
    Indent is 3,
	write(Att),nl,
	parseBigList(Rest, Indent).





% Induction of decision trees (program sketched on Bratko pages 466-468)
% Below, the sketched program was completed by adding some missing parts

% Note: This program does not pay any attention to efficiency!

     induce_tree( Tree)  :-
        findall( example( Class, Obj), example( Class, Obj), Examples),
        findall( Att, attribute( Att, _ ), Attributes),
        induce_tree( Attributes, Examples, Tree).

% The form of the tree depends on the following three cases:

% (1) Tree = null if the example set is empty.
% (2) Tree = leaf( Class) if all of the examples are of the same class Class.
% (3) Tree = tree( Attribute, [ Val1 : SubTree1, Val2 : Subtree2, ...]) if examples
%     belong to more than one class,
%     Attribute is the root of the tree,
%     Val1, Val2, ... are Attribute's values, and
%     SubTree1, SubTree2, ... are the corresponding decision subtrees.

% These three cases are handled by the following three clauses:

% induce_tree( Attributes, Examples, Tree)

   induce_tree( _, [], null)  :-  !.

   induce_tree( _, [example( Class,_ ) | Examples], leaf( Class))  :-
     not(( member( example( ClassX, _), Examples),           % No other example
           ClassX \== Class)), !.                             % of different class

   induce_tree( Attributes, Examples, tree( Attribute, SubTrees))  :-
     choose_attribute( Attributes, Examples, Attribute), !,
     del( Attribute, Attributes, RestAtts),                  % Delete Attribute
     attribute( Attribute, Values),
     induce_trees( Attribute, Values, RestAtts, Examples, SubTrees).

   induce_tree( _, Examples, leaf( ExClasses))  :-     % No (useful) attribute, leaf with class distr.
     findall( Class, member( example( Class, _), Examples), ExClasses).

% induce_trees( Att, Values, RestAtts, Examples, SubTrees):
%   induce decision SubTrees for subsets of Examples according to Values of Attribute

   induce_trees( _, [], _, _, [] ).     % No attributes, no subtrees

   induce_trees( Att, [Val1 | Vals], RestAtts, Exs, [Val1 : Tree1 | Trees])  :-
     attval_subset( Att = Val1, Exs, ExampleSubset),
     induce_tree( RestAtts, ExampleSubset, Tree1),
     induce_trees( Att, Vals, RestAtts, Exs, Trees).

   del(Item, BigList, SmallList) :- delete(BigList, Item, SmallList).

% attval_subset( Attribute = Value, Examples, Subset):
%   Subset is the subset of examples in Examples that satisfy the condition Attribute = Value:

   attval_subset( AttributeValue, Examples, ExampleSubset)  :-
     findall( example( Class, Obj),
              ( member( example( Class, Obj), Examples),
                satisfy( Obj, [ AttributeValue])),
              ExampleSubset).

% satisfy( Object, Description):
%   defined as in Figure 18.11.

satisfy( Object, Conj)  :-
   not(( member( Att = Val, Conj),
         member( Att = ValX, Object),
         ValX \== Val)).

% choose_attribute selects the attribute that discriminates well among the classes.
% This involves the impurity criterion. The following clause minimizes the chosen
% impurity measure using setof.
% setof will order the available attributes according to increasing impurity

   /*choose_attribute( Atts, Examples, BestAtt)  :-
     setof( Impurity/Att,
            ( member( Att, Atts), impurity1( Examples, Att, Impurity)),
            [ _/BestAtt | _] ).*/

choose_attribute(Atts, Examples, BestAtt) :-
	choose_att(Examples, Atts, BestAtt).
 
choose_att(Examples, [A| Atts], BestAtt):-
	choose_att(Examples, Atts, A, BestAtt).
 
choose_att(_, [], BestAtt, BestAtt).
 
choose_att(Examples, [A| Atts], Test, BestAtt):-
	impurity1(Examples, A, Imp1),
	impurity1(Examples, Test, Imp2),
	%BestImp is min(Imp1, Imp2), is this bad?impurity1(Examples, Best, BestImp),
	Imp1 < Imp2,
	choose_att(Examples, Atts, A, BestAtt).
 
choose_att(Examples, [A| Atts], Test, BestAtt):-
	impurity1(Examples, A, Imp1),
	impurity1(Examples, Test, Imp2),
	Imp1 > Imp2,
	choose_att(Examples, Atts, Test, BestAtt).

/*
 choose_attribute( Atts, Examples, BestAtt)  :-
    bagof( Impurity/Att,
            ( member( Att, Atts), impurity1( Examples, Att, Impurity)),
            Bag),
     %write('Bag is '), write (Bag), nl,
     %write('Examples are '), write(Examples), nl,
     %write('Atts are '), write(Atts), nl,
     %write('choose '),
     min_member(_/BestAtt, Bag), write(BestAtt), nl.

*/
% impurity1( Examples, Attribute, Impurity):
%   implements a chosen impurity measure.
%   Impurity is the combined impurity of the subsets of examples after
%   dividing the list Examples according to the values of Attribute.
%

impurity1( Exs, Att, Imp)  :-
  attribute( Att, AttVals),
  term_sum( Exs, Att, AttVals, 0, Imp).  % Weighted sum of impurities over att. values

% term_sum( Exs, Att, AttVals, PartialSum, Sum)

term_sum( _, _, [], Sum, Sum).

term_sum( Exs, Att, [Val | Vals], PartSum, Sum)  :-
  length( Exs, N),
  findall( C, (member( example(C,Desc), Exs), satisfy( Desc, [Att=Val])), ExClasses),
     % ExClasses = list of the classes (with repetitions) of all the examples with Att=Val
  length( ExClasses, NV), NV > 0, !,
  findall( P,
           ( bagof( 1, member( _, ExClasses), L), length( L, NVC), P is NVC/NV),
           ClassDistribution),
  gini( ClassDistribution, Gini),
  NewPartSum is PartSum + Gini*NV/N,
  term_sum( Exs, Att, Vals, NewPartSum, Sum)
  ;
  term_sum( Exs, Att, Vals, PartSum, Sum).      % Here no example satisfied Att = Val

% gini( ProbabilityList, GiniIndex)
%    GiniIndex = SUM Pi*Pj over all i, j such that i \= j
%    This is equivalent to:
%    GiniIndex = 1 - SUM Pi*Pi over all i

gini( Probs, Index)  :-
  square_sum( Probs, 0, SquareSum),
  Index is 1 - SquareSum.

square_sum( [], S, S).

square_sum( [P | Ps], PartS, S)  :-
  NewPartS is PartS + P*P,
  square_sum( Ps, NewPartS, S).


%-----------------------------------------

% Figure 18.9  Attribute definitions and examples for learning to
% recognize objects from their silhouettes (from Figure 18.8).

:- dynamic example/2.


attribute( size, [ small, large]).
attribute( shape, [ long, compact, other]).
attribute( holes, [ none, 1, 2, 3, many]).

% Tree from spec

example( nut, [ size = small, shape = compact, holes = 1]).
example( screw, [ size = small, shape = long, holes = none]).
example( key, [ size = small, shape = long, holes = 1]).
example( nut, [ size = small, shape = compact, holes = 1]).
example( key, [ size = large, shape = long, holes = 1]).
example( screw, [ size = small, shape = compact, holes = none]).
example( nut, [ size = small, shape = compact, holes = 1]).
example( pen, [ size = large, shape = long, holes = none]).
example( scissors, [ size = large, shape = long, holes = 2]).
example( pen, [ size = large, shape = long, holes = none]).
example( scissors, [ size = large, shape = other, holes = 2]).
example( key, [ size = small, shape = other, holes = 2]).



% Smaller tree with height = 2
/*
example( screw, [ size = small, shape = long, holes = none]).
example( screw, [ size = small, shape = compact, holes = none]).
example( pen, [ size = large, shape = long, holes = none]).
example( pen, [ size = large, shape = long, holes = none]).
*/







