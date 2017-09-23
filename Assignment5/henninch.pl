% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isFather(X) :- parent(X,_), male(X).
isMother(X) :- parent(X,_), female(X).

% 3. Define a predicate `grandparent/2`.
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z,Y), parent(Z,X), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- sibling(X,Y), male(X).
sister(X,Y) :- sibling(X,Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- married(X,Z), sibling(Z,Y).
siblingInLaw(X,Y) :- sibling(X,Z), married(Y,Z).


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- sibling(X,Z), parent(Z,Y), female(X).
aunt(X,Y) :-  siblingInLaw(X,Z), parent(Z,Y), female(X).
uncle(X,Y) :- sibling(X,Z), parent(Z,Y), male(X).
uncle(X,Y) :- siblingInLaw(X,Z), parent(Z,Y), male(X).

% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- sibling(W,V), parent(W,X), parent(V,Y).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).

% Extra credit: Define the predicate `related/2`.

related(X,Y) :- male(Y), X \= Y.
related(X,Y) :- female(Y), X \= Y.



%%
% Part 2. Language implementation (see course web page)
%%

% 1. `cmd/3`
add(X,Y,Z)           :- Z is X + Y.
lteTrue(X,Y)         :- X =< Y. 
lteFalse(X,Y)        :- X > Y. 
str(S)               :- string(S).
num(S)               :- number(S).
cmd(X,S1,S2)         :- num(X), S2= [X|S1].
cmd(X,S1,S2)         :- str(X), S2 = [X|S1].
cmd(add,[X,Y|S1],S2) :- add(X, Y, Z), S2 = [Z|S1].
cmd(lte,[X,Y|S1],S2) :- lteTrue(X,Y), S2 = [t|S1].
cmd(lte,[X,Y|S1],S2) :- lteFalse(X,Y), S2 = [f|S1].

% 2. `prog\3`


isT(t)                  :- true.
isf(f)                  :- true.
prog([],S1,S2)          :- S2 = S1.
prog([A|X],S1,S2)       :- num(A),append([A],S1,Y), prog(X,Y,S2).
prog([A|X],S1,S2)       :- str(A),append([A],S1,Y), prog(X,Y,S2).
prog([add|X],[A,B|S1],S2)     :- add(A,B,Z), append([Z],S1,Y), prog(X,Y,S2).
prog([lte|X],[A,B|S1],S2) :- lteTrue(A,B), append([t],S1,Y), prog(X,Y,S2).
prog([lte|X],[A,B|S1],S2) :- lteFalse(A,B), append([f],S1,Y), prog(X,Y,S2).
prog([if([P1],[P2])|X],[t|S1],S2) :- append([P1],S1,Y), prog(X,Y,S2).
prog([if([P1],[P2])|X],[f|S1],S2) :- append([P2],S1,Y), prog(X,Y,S2).
prog([if(P1,P2)|X],[A|S1],S2) :- prog(X,Y,S2).


