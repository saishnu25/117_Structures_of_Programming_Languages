%Saishnu Ramesh Kumar (300758706)
%CSCI 117 - Lab 8

%-- PART 1 --

% mode (+,+,-)
% ?- cons(5, snoc(snoc(nil,3),4), X).       
% Solution X = snoc(snoc(snoc(nil,5),3),4)
cons(E, nil, snoc(nil,E)).
cons(E, snoc(BL,N), snoc(EBL,N)) :- cons(E, BL, EBL).

%mode(+,+,-)
% ?- snoc([1,2,3],5,O). 
snoc([],E, [E]). 
snoc([Y|YS], X, [Y|Out]):-snoc(Ys,X,Out). 

%mode(+,-)
% ?- fromBList(snoc(snoc(snoc(nil,1),2),3),Out).
snoc([],E,[E]). 
snoc([Y|Ys],X,[Y|Out]) :-snoc(Ys,X,Out). 
fromBList(nil,[]). 
fromBList(snoc(L,N),Out):-fromBList(L,Y),snoc(Y,N,Out). 

%mode(+,-)
% ?- toBList([1,2],Out).
cons(nil,E,snoc(nil,E)).
cons((snoc(BL,N)),E,snoc(BLN,N)):-cons(BL,E,BLN).
toBList([],nil). 
toBList([X|Xs],Out):-toBList(Xs,Y), cons(Y,X,Out).  

%mode(+,+,-)
% ?- num_empties(node(1,empty,empty),E). 
num_empties(empty,1). 
num_empties(node(A,T1,T2),Z) :-num_empties(T1,X),num_empties(T2,Y),Z is X+Y.

%mode(+,+,-)
% ?= num_nodes(node(1,empty,empty),E).
num_nodes(empty,0). 
num_nodes(node(A,T1,T2),Z):-num_nodes(T1,X),num_nodes(T2,Y),Z is 1+X+Y. 

%mode(+,+,-)
% ?-insert_left(1,node(2,empty,empty),E). 
insert_left(X,empty,node(X,empty,empty)). 
insert_left(X,node(A,T1,T2),node(A,TT1,T2)):-insert_left(X,T1,TT1). 

%mode(+,+,-)
% ?- insert_right(1,node(2,empty,empty),E). 
insert_right(X,empty,node(X,empty,empty)). 
insert_right(X,node(A,T1,T2),node(A,T1,TT2)):-insert_right(X,T2,TT2). 

%mode(+,+,-)
% ?- sum_nodes(node(node(empty,empty,3),empty,5),O).
sum_nodes(empty,0). 
sum_nodes(node(A,T1,T2),Z):-sum_nodes(T1,X),sum_nodes(T2,Y),Z is A+X+Y. 

%mode(+,+,-)
% ?- inorder(node(3,empty,empty),O). 
append([],Y,Y). 
append([H|T],Y,[H|Z]):-append(T,Y,Z). 
inorder(empty,[]). 
inorder(node(A,T1,T2),Out2):-
inorder(T1,TT1),inorder(T2,TT2),append(TT1,[A],Out1),append(Out1,TT1,Out2). 

%mode(+,-)
% ?- num_elts(node(1,leaf(1),leaf(3)),O). 
num_elts(leaf(A),1). 
num_elts(node(A,L,R),Z):-num_elts(L,X),num_elts(R,Y),Z is 1+X+Y. 

%mode(+,-)
% ?- sum_nodes2(node(1,leaf(1),leaf(3)),0)
sum_nodes2(leaf(A),1). 
sum_nodes2(node(A,L,R),Z):-sum_nodes2(L,X),sum_nodes2(R,Y),Z is A+X+Y. 

%mode (+,-)
% ?- inorder2(node(1,leaf(4),leaf(1)),O)
append([],Y,Y).
append([H|T],Y,[H|Z]) :- append(T,Y,Z).
inorder2(leaf(A),[A]).
inorder2(node(A,T1,T2),Out2) :-
inorder2(T1,TT1),inorder2(T2,TT2),append(TT1,[A],Out1),append(Out1,TT1,Out2).

%mode (+,-)
% ?- conv21((node2(3,leaf(5),leaf(6))),Out)
conv21(leaf(A),node(A ,empty, empty)).
conv21(node2(X,T1,T2),node(X,TT1,TT2)):- conv21(T1,TT1),conv21(T2,TT2).

%-- PART 2 --

%mode (+,-)
% ?- toBList_it([1,2],O)
toBList_it(List,O):- toBList_it(List,nil,O).
toBList_it([],A,A).
toBList_it([X|Xs],A,AN):-
    AP = snoc(A,X),
    toBList_it(Xs,AP,AN).

%mode (+,-)
% ?- fromBlist(snoc(snoc(snoc(nil,4),5),6),O)
fromBlist(snoc(L,N),O):- fromBList_it(snoc(L,N),[],O).
fromBlist_it(nil,A,A).
fromBlist_it(snoc(L,N),A,AN):-
    AP = [N|A], 
    fromBlist_it(L,AP,AN).

%mode (+,-)
% ?- num_empties_it((node(2,empty,empty)),Out)
num_empties_it(T,Out1):- num_empties_it_H([T],0,Out1).
num_empties_it_H([],A,A).
num_empties_it_H([empty|Ts],A,Out1):- AP is A+1, num_empties_it_H(Ts, AP, Out1).
num_empties_it_H([node(X,T1,T2)|Ts],A,Out1):-num_empties_it_H([T1,T2|Ts],A,Out1).

%mode (+,-)
% ?- num_nodes((node(2,empty,empty)),Out)
num_nodes(T,Out1):- num_nodes_it([T],0,Out1).
num_nodes_it([],A,A).
num_nodes_it([empty|Ts],A,Out1):- num_nodes_it(Ts,A,Out1).
num_nodes_it([node(X,T1,T2|Ts],A,Out1):-AP is A+1, num_nodes_it(Ts,AP,Out1).          
             
%mode (+,-)
% ?- sum_nodes2(leaf(2),Out)
sum_nodes2(T,N):- sum_nodes2_it([T],0,N).
sum_nodes2_it([],A,A). 
sum_nodes2_it([leaf(E)|Ts],A,N):- AP is E+A, sum_nodes2_it(Ts,AP,N).
sum_nodes2_it([node2(X,T1,T2)|Ts],A,N):- AP is A+X, sum_nodes2_it([T1,T2|Ts],AP,N).

%mode (+,-)
% ?- inorder2_it_H(node(X,L,R)|Ts],A,Out)
inorder2_it(T,Out1):- inorder2_it_H([T],[],Out1).
inorder2_it_H([],A,A).
inorder2_it_H([leaf(F)|Ts],A, Out1) :- inorder2_it_H(Ts,[F|A],Out1).
inorder2_it_H([node2(X,T1,T2)|Ts],A,Out1):- inorder2_it_H([T2,leaf(X),T1|Ts],A,Out1).
             
%mode(+,-)
% ?- bst node(10,node(8,empty,node(9,empty,empty)),
node((11,empty,node(12,empty,empty)),0).
bst(empty,true).
bst(T):-bst_it(T,neginf,posinf).             
bst_it(empty,_,_).
bst_it(node(X,L,R),low,high):-lt(low,fin(X)),lt(fin(X),high),bst_it(L,low,fin(X)),bst_it(R,fin(X),high).            

%mode(+,-)
% ?- bst2 node(10,node(8,empty,node(9,empty,empty)),
node((11,empty,node(12,empty,empty)),O).
bst2(leaf(X),true).
bst2(T):-bst2_it(T,neginf,posinf).
bst2_it(leaf(X),low,high):-lt(low,fin(X)),lt(fin(X),high).             
bst2_it(node(X,L,R),low,high):-
    lt(low,fin(X)),lt(fin(X),high),bst2_it(L,low,fin(X)),bst2_it(R,fin(X),high).

snoc:
%mode (+,+,-)
%?-snoc(X,Y,[1,5])
%mode (-,+,+)
%?- snoc(X,5,[1,5])
%mode (-,-,+)
%?- snoc(X,Y,[1,3,5])
%mode (+,-,+)
%?- snoc([1,2],X, [1,5])

fromBList:
%mode (+,-)
%?-fromBList(snoc(snoc(snoc(nil,1),2),3),X).

toBList:
%mode (+,-)
toBList([1,2],X).

num_empties:
%mode (+,-)
?-num_empties(node(1,empty,empty),X)
num_nodes:
%mode (+,-)
?-num_nodes(node(1,empty,empty),X)

insert_left:
%mode (+,+,-)
?-insert_left(3,(node(2,empty,empty)),Out)
%mode (-,+,+)
?-insert_left(X,(node(2,empty,empty)), node(2, node(3, empty, empty), empty))
%mode (+,-,+)
?- insert_left(3,Y, node(2, node(3, empty, empty), empty))
%mode(-,+,-)
?- insert_left(X,node(2, empty, empty), Z)
%mode(-,-,+)

insert_right:
%mode (+,+,-)
?- insert_right(3,(node(2,empty,empty)),Out)
%mode (-,+,+)
?- insert_right(X,(node(2,empty,empty)), node(2, node(3, empty, empty), empty))
%mode (+,-,+)
?- insert_right(3,Y, node(2, node(3, empty, empty), empty))
%mode(-,+,-)
?- insert_right(X,node(2, empty, empty), Z)
%mode(-,-,+)

sum_nodes:
%mode (+,-)
?- sum_nodes2(node(1,leaf(1),leaf(3)),X)

inorder:
%mode (+,-)
?-inorder2(node(1,leaf(4),leaf(1)),O)

num_elts:
%mode(+,-)
num_elts(node(1,leaf(1),leaf(3)),X)

sum_nodes2:
%mode (+,-)
?-sum_nodes2(node(1,leaf(1),leaf(3)),X)

inorder2:
%mode (+,-)
?-inorder2(node(1,leaf(4),leaf(1)),X)

conv21:
%mode (+,-)
?- conv21((node2(3,leaf(5),leaf(6))),X)

toBList_it:
%mode (+,-)
% ?- toBList_it([1,2],O)

fromBList_it:
%mode (+,-)
?-fromBList(snoc(snoc(snoc(nil,1),2),3),X)

num_empties_it:
%mode (+,-)
?-num_empties_it((node(2,empty,empty)),X)

num_nodes_it:
% mode (+,-)
?-num_nodes((node(2,empty,empty)),X)

sum_nodes2_it:
%mode (+,-)
?-sum_nodes2(leaf(2),X)

inorder2_it:
%mode (+,-)
?-inorder2_it_H(node(X,L,R)|Ts],A,X)   

%-- PART 4 (EXTRA CREDIT SECTION) --

bst:
%mode(+,-)
?-bst node(10,node(8,empty,node(9,empty,empty)), node(11,empty,node(12,empty,empty)),X). 

bst2:
%mode(+,-)
?-bst node(10,node(8,empty,node(9,empty,empty)), node(11,empty,node(12,empty,empty)),X).









