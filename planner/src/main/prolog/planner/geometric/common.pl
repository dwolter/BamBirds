:- module(geometric_common, [point_line_rel/7, some_point_on_side/6, distance/3]).

point_line_rel(PX, PY, XS, YS, XE, YE, R) :-
	S is (PX-XS)*(-YE+YS) + (PY-YS)*(XE-XS),
	(S>0 -> R=l; R=r).

% succeeds if one point [X,Y] of POINTS lies on Side (l or r) of line X1,Y1 -> X2,Y2
some_point_on_side(POINTS, X1, Y1, X2, Y2, Side) :-
	member([PLX, PLY], POINTS),
	point_line_rel(PLX, PLY, X1, Y1, X2, Y2, Side),
	!.

distance([X1, Y1], [X2, Y2], Distance) :-
	Distance is sqrt((X2-X1)**2 + (Y2 -Y1)**2).
