:- module(shot_collision_shapes, [
	col_shape/6, 
	generate_collision_shapes/0,
	purge_collision_shapes/0,
	object_on_parabola/5,
	object_on_parabola/7
	]).
:- use_module(planner(ab/relations)).
:- use_module(planner(geometric/polygon)).
:- use_module(planner(geometric/intersection)).
:- use_module(planner(data)).


%%%
%%% enlarging polygons for shot planning
%%%
:- dynamic(col_shape/6).

assert_collision_shape(Object, poly, CX, CY, A, [N | POINTS], RADIUS) :-
	extend_polygon(POINTS, RADIUS, EXT_POINTS),
	assertz(col_shape(Object, poly, CX, CY, A, [N | EXT_POINTS])).

assert_collision_shape(Object, ball, CX, CY, A, [R], RADIUS) :-
	R2 is (R + RADIUS),
	assertz(col_shape(Object, ball, CX, CY, A, [R2])).

assert_collision_shape(Object, rect, CX, CY, A, [H,W,Angle], RADIUS) :-
	H2 is (H + (RADIUS * 2)),
	W2 is (W + (RADIUS * 2)),
	assertz(col_shape(Object, rect, CX, CY, A, [H2, W2, Angle])).

assert_collision_shape(_, unknown, _, _, _, _, _). 

bird_safety_margin(black, RB, R) :-
	R is (max(RB, 10)).

bird_safety_margin(white, RB, R) :-
	R is (max(RB, 10)).

bird_safety_margin(_, R, R2) :-
	R2 is (R+1).

current_bird_safety_margin(M) :-
	in_slingshot(B0),
	hasColor(B0, BC),
	((shape(B0, ball, _, _, _, [RB]), R is RB+1) ; R is 8),
	bird_safety_margin(BC, R, M).

generate_collision_shapes :-
	current_bird_safety_margin(M),
	forall(shape(Object, Type, CX, CY, Area, ShapeData),
		assert_collision_shape(Object, Type, CX, CY, Area, ShapeData, M)),!.

purge_collision_shapes :-
	retractall(col_shape(_,_,_,_,_,_)).

% succeeds if some points are left and some are right of line
%crosses(_, _, _, _, _, true, true).
%crosses(XS, YS, XE, YE, [[PX,PY]|PS], true, R) :-
%    point_line_rel(PX, PY, XS, YS, XE, YE, left),
%    crosses(XS, YS, XE, PS, true, R).

%crosses(XS, YS, XE, YE, [[PX,PY]|PS], L, true) :-
%    point_line_rel(PX, PY, XS, YS, XE, YE, right),
%    crosses(XS, YS, XE, PS, L, true).

% secceeds if object intersects line segment
%object_on_line(Obj, XStart, YStart, XEnd, YEnd) :-
%    (hasMaterial(Obj,_,X,Y,W,H) ; hill(Obj, X,Y,W,H)),
%    ((XStart =< X+W, X =< XEnd) ; (YStart =< Y+H, Y =< YEnd)). % bounding boxes intersect
%    crosses(XStart, YStart, XEnd, YEnd, [[PX,PY],[PX+W,PY], [PX+W,PY+H], [PX,PY+H]], false, false)%.

object_on_parabola(Obj,DXMAX,A,B,HITX) :-
	slingshotPivot(X0,Y0),
	object_on_parabola(Obj, X0, Y0, DXMAX, A, B, HITX).

object_on_parabola(Obj, X0, Y0, DXMAX, A, B, HITX) :-
	(hasMaterial(Obj,_,X, _Y, W, _H) ;  % X,Y ist Ecke links oben; +W, +H rechts unten
	 hill(Obj, X, _Y, W, _H) ),
	DX is X - X0,
	current_bird_safety_margin(M),
	(DX+W+M) > 0, % Object not left of slingshot
	(DX-M) < DXMAX, %% Object not behind target

	% Uhoh: the following test only compares bounding box of object to Y coordinates of the parabola
	% on the left and at the right side of the box. This ignores the possibility that the parabola
	% has a climax right in the middle.
	% FIXME: we need to gate  the test such that the parabola has the sae slope on the left and on the
	% right side of the box. For time being we leave the test commented out, so computation is correct
	% at the cost of doing some line/parabola intersections that could have been avoided.
	%
	%PY is Y0 - A*DX*DX - B*DX, % parabola at left end of box
	%DX_BACK is min(DXMAX, X + W - X0),
	%PY_BACK is Y0 - A*DX_BACK*DX_BACK - B*DX_BACK, % parabola at right end of box
	%writeln(['o_o_p', Obj,X,Y,W,H,[DX,PY],[DX_BACK,PY_BACK]]),
	%YL is Y+H,
	%( (Y < PY, PY < YL) ; (PY < Y, PY_BACK > Y) ; (PY < Y, PY_BACK > YL) ; (PY > YL, PY_BACK < Y) ),
	%shape(Obj, ShapeType, CX, CY, _, ShapeData),
	col_shape(Obj, ShapeType, CX, CY, _, ShapeData),  %%% <--- FIXME: are we sure about this?
	%writeln(['p_c_s', ShapeType, CX, CY, ShapeData, X0, Y0, DXMAX, A, B]),
	parabola_crosses_shape(ShapeType, CX, CY, ShapeData, X0, Y0, DXMAX, A, B, HITX),
	HITX>X0.
