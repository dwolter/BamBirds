:- module(geometric_intersection, [
	line_poly_intersection/7,
	parabola_crosses_shape/10,
	line_crosses_shape/10,
	object_on_vline/5,
	line_crosses_bb/5
	]).
:- use_module(planner(data)).
:- use_module(polygon).
:- use_module(common).
:- use_module(planner(physics/projectile_motion)).



% succeeds if line segments [X1S, Y1S] -> [X1E,X1E] and [X2S, Y2S] -> [X2E, Y2E] intersect,
% IX, IY are unified with intersection point
% formula from https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
lines_intersect(X1S,Y1S,X1E,Y1E,X2S,Y2S,X2E,Y2E, IX, IY) :-
	S1_X is X1E - X1S,
	S1_Y is Y1E - Y1S,
	S2_X is X2E - X2S,
	S2_Y is Y2E - Y2S,
	DIV1 is (-S2_X * S1_Y + S1_X * S2_Y),
	DIV2 is (-S2_X * S1_Y + S1_X * S2_Y),
%    format('DIV1=~w~nDIV2=~w~nsegments=~w  and ~w~n~n', [DIV1, DIV2, [X1S,Y1S,X1E,Y1E], [X2S,Y2S,X2E,Y2E]]),
	abs(DIV1) >= 0.00001,
	abs(DIV2) >= 0.00001,
	 S is (-S1_Y * (X1S - X2S) + S1_X * (Y1S - Y2S)) / DIV1,
	S >= 0,
	S =< 1,
	T is ( S2_X * (Y1S - Y2S) - S2_Y * (X1S - X2S)) / DIV2,
	T >= 0,
	T =< 1,
	IX is X1S + T*S1_X,
	IY is Y1S + T*S1_Y.

%% test line segment intersection on boundary, passes first point as 2nd argument
%% to allow loop-closing
line_boundary_intersect(X1S,Y1S,X1E,Y1E, [X0,Y0], [[XN, YN]], IX, IY) :-
	lines_intersect(X1S,Y1S,X1E,Y1E, XN,YN, X0, Y0, IX, IY).
line_boundary_intersects(X1S,Y1S,X1E,Y1E, _, [[X0, Y0], [X1, Y1] | _], IX, IY) :-
	lines_intersect(X1S,Y1S,X1E,Y1E, X0, Y0, X1, Y1, IX, IY).
line_boundary_intersects(X1S,Y1S,X1E,Y1E, LastPoint, [_ | PRest], IX, IY) :-
	line_boundary_intersects(X1S,Y1S,X1E,Y1E, LastPoint, PRest, IX, IY).
%% convenience predicate to check line segment to polygon (= list of points)
%% intersection
line_poly_intersection(X1S,Y1S,X1E,Y1E, [P1 | Prest], IX, IY) :-
	line_boundary_intersects(X1S,Y1S,X1E,Y1E, P1, [P1 | Prest], IX, IY).


%% check intersection of parabola and polygon
line_parabola_intersection([X1,Y1], [X2,Y2], X0, Y0, DXMAX, A, B, X1) :-
	abs(X1-X2) < 3, % OK for nearly vertical and much faster than precise check
	X1 =< DXMAX+X0,
	!,
	YP is Y0 - A*(X1-X0)^2 - B*(X1-X0),
	%format('vertical check [~w,~w], [~w,~w] ... YP=~w~n', [X1,Y1,X2,Y2,YP]),
	YP < max(Y1,Y2),
	YP > min(Y1,Y2).

line_parabola_intersection([X1,Y1], [X2,Y2], X0, Y0, DXMAX, A, B, HX) :-
	abs(X1-X2) > 2,  % \= 0 is important
	abs(A)>1e-10,
	!,
	M is (Y2-Y1)/(X2-X1),
	P is (2*A*X0 - B - M)/ (-A),
	Q is (Y0-A*X0*X0+B*X0-Y1+M*X1)/ (-A),
	ROOTTERM is 0.25*P^2 - Q,
	ROOTTERM >= 0,
	IX1 is (-0.5 * P) - sqrt(ROOTTERM),
	IX2 is (-0.5 * P) + sqrt(ROOTTERM),
	!,
	member(HX, [IX1,IX2]),
	HX =< DXMAX+X0,
	HX >= min(X1,X2),
	HX =< max(X1,X2).


poly_parabola_intersection(P0, [PN], X0, Y0, DXMAX, A, B, HX) :-
	line_parabola_intersection(P0, PN, X0, Y0, DXMAX, A, B, HX).

poly_parabola_intersection(_, [P1, P2 | _], X0, Y0, DXMAX, A, B, HX) :-
	line_parabola_intersection(P1, P2, X0, Y0, DXMAX, A, B, HX).

poly_parabola_intersection(P0, [_, P2 | PREST], X0, Y0, DXMAX, A, B, HX) :-
	poly_parabola_intersection(P0, [P2 | PREST], X0, Y0, DXMAX, A, B, HX).

%%
%% true shape intersection test shape/parabola
%%
%%


parabola_crosses_shape(rect, CX, CY, [W,H,Angle], X0, Y0, DXMAX, A, B, HITX) :-
	%% width (W) is in Y direction @ ANGLE=0
	%% length (H) is in X direction @ ANGLE=0
	%% NB: rotation is clock-wise!
	%% X = x*cos(θ) - y*sin(θ)
	%% Y = x*sin(θ) + y*cos(θ)
	XR is (0.5*H),
	YR is (0.5*W),
	rot_shift(Angle, CX, CY, [[-XR,-YR], [-XR,YR], [XR,YR], [XR,-YR]], POINTS),
	POINTS = [P1 | _],
	findall(HX, poly_parabola_intersection(P1, POINTS, X0, Y0, DXMAX, A, B, HX), HITXS),
	min_list(HITXS, HITX).

parabola_crosses_shape(ball, CX, CY, [R], X0, Y0, _, A, B, HX) :- % FIXME: add true shape test
	X is CX-X0,
	YP is Y0 - A*X*X - B*X,
	Y is YP-CY,
	abs(Y) =< R,
	M is 2*A*X + B,
	ATERM is 1+M*M,
	BTERM is 2*M*Y,
	ROOTTERM is BTERM*BTERM - 4*ATERM*(Y*Y-R*R),
	HX is CX + (-BTERM - sqrt(ROOTTERM)) / (2*ATERM).

parabola_crosses_shape(poly, _, _, [_ | [P1 | POINTS]], X0, Y0, DXMAX, A, B, HX) :-
	findall(HX, poly_parabola_intersection(P1, [P1 | POINTS], X0, Y0, DXMAX, A, B, HX), HXS),
	min_list(HXS, HX).
parabola_crosses_shape(unknown, CX, _, _, _, _, _, _, _, CX). 


poly_line_intersection([X1,Y1], [[XN,YN]], LX1, LY1, LX2, LY2, HITX, HITY) :-
	lines_intersect(X1,Y1,XN,YN,  LX1, LY1, LX2, LY2, HITX, HITY).

poly_line_intersection(_, [[X1,Y1], [X2,Y2] | _], LX1, LY1, LX2, LY2, HITX, HITY) :-
	lines_intersect(X1,Y1, X2,Y2, LX1, LY1, LX2, LY2, HITX, HITY).

poly_line_intersection(P1, [_| PS],  LX1, LY1, LX2, LY2, HITX, HITY) :-
	poly_line_intersection(P1, PS,  LX1, LY1, LX2, LY2, HITX, HITY).

line_crosses_shape(rect, CX, CY, [W,H,Angle], LX1, LY1, LX2, LY2, HITX, HITY) :-
	%% width (W) is in Y direction @ ANGLE=0
	%% length (H) is in X direction @ ANGLE=0
	%% NB: rotation is clock-wise!
	%% X = x*cos(θ) - y*sin(θ)
	%% Y = x*sin(θ) + y*cos(θ)
	XR is (0.5*H),
	YR is (0.5*W),
	rot_shift(Angle, CX, CY, [[-XR,-YR], [-XR,YR], [XR,YR], [XR,-YR]], POINTS),
	POINTS = [P1 | _],
%    writeln(POINTS),
	poly_line_intersection(P1, POINTS, LX1, LY1, LX2, LY2, HITX, HITY).

%% from https://math.stackexchange.com/questions/311921/get-location-of-vector-circle-intersection
line_crosses_shape(ball, CX, CY, [R], LX1, LY1, LX2, LY2, HITX, HITY) :-
	A is (LX2 - LX1)^2 + (LY2 - LY1)^2,
	B is 2*(LX2 - LX1)*(LX1 - CX) + 2*(LY2-LY1)*(LY1-CY),
	C is (LX1 - CX)^2 + (LY1 - CY)^2 - R^2,
	ROOTTERM is B^2-4*A*C,
	ROOTTERM >= 0,
	T1 is (sqrt(ROOTTERM) - B) / (2*A),
	T2 is (sqrt(ROOTTERM) + B) / (2*A),
	member(T, [T1,T2]),
	T>=0,
	T=<1,
	!,
	HITX is (LX2-LX1)*T + LX1,
	HITY is (LY2-LY1)*T + LY1.

line_crosses_shape(poly, _, _, [_ | [P1 | POINTS]], LX1,LY1,LX2,LY2, HITX, HITY) :-
	poly_line_intersection(P1, [P1 | POINTS], LX1, LY1, LX2, LY2, HITX, HITY).

object_on_vline(O,PX,YUP,YDOWN,HITY) :-
	(hasMaterial(O,_,OXL,OYUP,OW,OH) ; hill(O, OXL,OYUP,OW,OH)),
	OXL<PX,
	OXL+OW > PX,
	( (YDOWN >= OYUP, YDOWN =< OYUP+OH) ; (YUP >= OYUP, YUP =< OYUP+OH) ; (YUP =< OYUP, YDOWN >= OYUP+OH) ),
%    format('checking ~w further...~n', [O]),
	shape(O, ShapeType, CX, CY, _, ShapeData),
	line_crosses_shape(ShapeType, CX, CY, ShapeData ,PX,YUP,PX,YDOWN, _, HITY).

% succeeds if line X1,Y1->X2,Y2 crosses bounding box of object O
line_crosses_bb(O, X1, Y1, X2, Y2) :-
	(hasMaterial(O, _, XL, YU, W, H) ; hill(O, XL, YU, W, H)),
	XR is XL+W,
	YD is YU+H,
	POINTS = [[XL,YU], [XL, YD], [XR, YD], [XR, YU]],
	some_point_on_side(POINTS, X1, Y1, X2, Y2, l),
	some_point_on_side(POINTS, X1, Y1, X2, Y2, r).

% 2nd case: line fully contained in bounding box
line_crosses_bb(O, X1, Y1, _, _) :-
	(hasMaterial(O, _, XL, YU, W, H) ; hill(O, XL, YU, W, H)),
	XL < X1,
	X1 < XL+W,
	YU < Y1, 
	Y1 < YU+H.
