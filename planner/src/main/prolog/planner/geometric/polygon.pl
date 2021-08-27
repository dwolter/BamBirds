:- module(geometric_polygon, [extend_polygon/3, rot_shift/5]).


%%%
%%% enlarging polygons for shot planning
%%%

winding_number(_, [], A, A).
winding_number([X1,Y1], [[XN, YN]], A, E) :-
	E is (A + (X1 - XN) * (Y1 + YN)).
winding_number(P1, [[XI, YI], [XJ, YJ] | PTS], A, E) :-
	A2 is (A + (XJ - XI) * (YI + YJ)),
	winding_number(P1, [[XJ, YJ] | PTS], A2, E).

polygon_ccw([P1 | PN], OrderedPoly) :-
	winding_number(P1, [P1 | PN], 0, E),
	(E > 0.0 -> 
		OrderedPoly=[P1 | PN];
		reverse([P1 | PN], OrderedPoly)
	).

right_normal([X1, Y1], [X2, Y2], [NX, NY]) :-
	DX is (Y1 - Y2),
	DY is (X2 - X1),
	DL is (sqrt(DX*DX + DY*DY)),
	NX is (DX/DL),
	NY is (DY/DL).

translate([X, Y], [DX, DY], S, [X2, Y2]) :-
	X2 is (X + S*DX),
	Y2 is (Y + S*DY).

shift_polygon(FirstN, PrevN, [PN], [PNE], D) :-
	translate(PN, PrevN, D, PN_Temp),
	translate(PN_Temp, FirstN, D, PNE).

shift_polygon(FirstN, PrevN, [P1, P2 | PTS], [P1S, P2S | PTSS], D) :-
	right_normal(P1, P2, N),
	translate(P1, N, D, P1S_Temp),
	translate(P1S_Temp, PrevN, D, P1S),
	translate(P2, N, D, P2S),
	shift_polygon(FirstN, N, [P2 | PTS], PTSS, D).

extend_polygon(Points, D, [P1E, P2E | ExtendedPoints]) :-
	polygon_ccw(Points, [P1, P2 | OrderedPoints]),
	right_normal(P1, P2, FirstN),
	translate(P1, FirstN, D, P1E),
	translate(P2, FirstN, D, P2E),
	append(OrderedPoints, [P1], CycledPoints),
	shift_polygon(FirstN, FirstN, [P2 | CycledPoints], ExtendedPoints, D).

%% rotates by angle, then shifts to CX,CY
rot_shift(_Angle, _CX, _CY, [], []).
rot_shift(Angle, CX, CY, [[X,Y] | PS], [[XRS, YRS] | PRS]) :-
	XR is X*cos(Angle) - Y*sin(Angle),
	YR is X*sin(Angle) + Y*cos(Angle),
	XRS is XR+CX,
	YRS is YR+CY,
	rot_shift(Angle, CX, CY, PS, PRS).