:- module(common_list, [
	shortest_list/4, 
	prune_list/3, 
	append_lists/2
	]).


shortest_list([],Shortest,_, Shortest).
shortest_list([(List, Len) | Rest], _, SLen, A) :-
	Len < SLen,
	shortest_list(Rest, List, Len, A).
shortest_list([(_, Len) | Rest], Shortest, SLen, A) :-
	Len >= SLen,
	shortest_list(Rest, Shortest, SLen, A).

prune_list([],_,[]).
prune_list(_,0,[]).
prune_list([Head|List], MaxElements, [Head | Result]):-
	CurElements is MaxElements-1,
	prune_list(List, CurElements, Result).

append_lists([], []).
append_lists([L1 | LR], L) :-
	append_lists(LR, L2),
	append(L1,L2,L).