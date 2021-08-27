:- module(plans_lr_bounce, [bounce_last_resort/6, plans_common:plan_last_resort/2, plans_common:plan_last_resort/3]).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).

plans_common:plan_last_resort(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"simpleBounceLR", confidence:C, reasons:Pigs}) :-
	bounce_last_resort(Bird, Target, ImpactAngle, C, Pigs, Shot).
plans_common:plan_last_resort(Bird, Target, plan{bird:Bird, shot:Shot, target_object:FirstHit, impact_angle:ImpactAngle, strategy:"simpleBounceLR", confidence:C, reasons:[Target]}) :-
	bounce_last_resort(Bird, FirstHit, ImpactAngle, C, [Target], Shot).

%%%
%%% last resort: make bird bounce onto target
%%%
point_on_line(X, Y, LX1, LY1, LX2, LY2) :-
	DX is (LX1 - X),
	DY is (LY1 - Y),
	DLX is (LX2 - LX1),
	DLY is (LY2 - LY1),
	abs(DX*DLY-DY*DLX) < 1 .

point_on_pline_test(_, [[LX1, LY1], [LX2, LY2] | _], HITX, HITY, LX1, LY1, LX2, LY2) :-
	point_on_line(HITX, HITY, LX1, LY1, LX2, LY2), !.

point_on_pline_test(P1, [_, P2 | POINTS], HITX, HITY, LX1, LY1, LX2, LY2) :-
	point_on_pline_test(P1, [P2 | POINTS], HITX, HITY, LX1, LY1, LX2, LY2).

point_on_pline_test([LX2,LY2], [[LX1,LY1]], HITX, HITY, LX1, LY1, LX2, LY2) :-
	point_on_line(HITX, HITY, LX1, LY1, LX2, LY2).

point_on_line(rect, [W,H,Angle], CX, CY, HITX, HITY, LX1, LY1, LX2, LY2) :-
	%% width (W) is in Y direction @ ANGLE=0
	%% length (H) is in X direction @ ANGLE=0
	%% NB: rotation is clock-wise!
	%% X = x*cos(theta) - y*sin(theta)
	%% Y = x*sin(theta) + y*cos(theta)
	XR is (0.5*H),
	YR is (0.5*W),
	rot_shift(Angle, CX, CY, [[-XR,-YR], [-XR,YR], [XR,YR], [XR,-YR]], POINTS),
	%    writeln(POINTS),
	point_on_line(poly, [0, POINTS], CX, CY, HITX, HITY, LX1, LY1, LX2, LY2).

point_on_line(poly, [_, P1 | POINTS], _, _, HITX, HITY, LX1, LY1, LX2, LY2) :-
	point_on_pline_test(P1, POINTS, HITX, HITY, LX1, LY1, LX2, LY2).

% for sorting points by proximity to X,Y
proxOrder(R, [_, X, Y, HITX1, HITY1, _], [_, _, _, HITX2, HITY2, _]) :-
	D1 is (sqrt((X-HITX1)^2 + (Y-HITY1)^2)),
	D2 is (sqrt((X-HITX2)^2 + (Y-HITY2)^2)),
	compare(R, D1, D2).

% compute the next object and location (OX,OY) it gets hit if following
% a line originating at X1,Y1 in direction Angle.
next_hit(Origin, X1, Y1, Angle, Object, OX, OY, DIR) :-
	X2 is (X1 + 200*sin(Angle)),
	Y2 is (Y1 + 200*cos(Angle)),
%   writeln([X1,Y1, X2, Y2]),
	findall([O, X1, Y1, HITX, HITY, LDIR],
		(line_crosses_bb(O, X1, Y1, X2, Y2),
%	     writeln(O),
		 O \= Origin,
		 col_shape(O, ShapeType, CX, CY, _ ,ShapeData),
%	     writeln(O),
		 line_crosses_shape(ShapeType, CX, CY, ShapeData, X1, Y1, X2, Y2, HITX, HITY),
		 point_on_line(ShapeType, ShapeData, CX, CY, HITX, HITY, LX1, LY1, LX2, LY2),
		 LDIR is (atan2(LY2-LY1, LX2-LX1))
%	     writeln([HITX, HITY, LX1, LY1, LX2, LY2, LDIR])
		),
		HITS),
%    writeln(HITS),
	predsort(proxOrder, HITS, SORTEDHITS),
	SORTEDHITS = [ [Object, _, _, OX, OY, DIR] | _ ].

%% succeeds if shooting at Object at OX,OY may bounce towards TX,TY
%% considering the angle only
can_bounce_off(OX, OY, TX, TY, Para_A, Para_B, DIR, Conf) :-
	slingshotPivot(X0, _),
	SDY is (-2*Para_A*(OX-X0) - Para_B),
	SL is (sqrt(SDY*SDY+1.0)),
	TDX is (OX - TX),
	TDY is (OY - TY),
	TL is (sqrt(TDX*TDX+TDY*TDY)),
	MX is (1.0/SL + TDX/TL),
	MY is (SDY/SL + TDY/TL),
	ML is (sqrt(MX*MX+MY*MY)),
	ML > 0,
	DA is (abs(180.0*acos(MX/ML*cos(DIR)+MY/ML*sin(DIR))/pi)),
%    writeln([SDY,SL,TDX,TDY,TL,MX,MY, DA]),
	((abs(90-DA) < 10, TL<50) ->
 Conf is 1.0 ; Conf is 0.51),
	abs(90-DA) < 30.
%    writeln([DIR, OX, OY, TX, TY, DA]).


% we can bounce onto the target if there's a straight line to a reflection point
% on a hill target. For such lines we need to check that the reflection point is
% hittable
bounce_last_resort(Bird, Object, ShotAngle, Conf, [Target], shot{sling_x:X0_INT, sling_y:Y0_INT, drag_x:RX, drag_y:RY, target_x:OX, target_y:OY, tap_time:TAP_INT}) :-
	hasColor(Bird, C),
	member(C, [red,blue,yellow]), 
	is_goal(Target, _),
	\+ isHittable(Target,_),
	hasMaterial(Target, _, XL, YT, W, H),
	X is (XL + 0.5*W),
	Y is (YT + 0.5*H),
	member(Angle, [1.5708, 1.6581, 1.7453, 1.8326, 1.9199, 2.0071, 2.0944, 2.1817, 2.2689, 2.3562, 2.4435, 2.5307, 2.6180, 2.7053, 2.7925, 2.8798, 2.9671, 3.0543, 3.1416, 3.2289, 3.3161, 3.4034, 3.4907, 3.5779, 3.6652, 3.7525, 3.8397, 3.9270, 4.0143, 4.1015, 4.1888, 4.2761, 4.3633, 4.4506, 4.5379, 4.6251, 4.7124]),
%   writeln([Angle,X,Y]),
	next_hit(Target, X, Y, Angle, Object, OX, OY, DIR),
%    writeln([Angle, Object, Angle, OX, OY]),
%    OXX is OX*0.05,
%		OYY is -0.05*OY,
%   writeln([OXX, OYY]),
	hill(Object),
	shots_at_point(Bird, OX, OY, Shots),
	member([_,_,A_RAD, Para_A, Para_B, Obstacles,TAP,RX,RY], Shots), % find shot...
%    writeln(['obstacles', A_RAD, Obstacles]),
	( Obstacles=[] ; (Obstacles=[[Object,BX, BY] | _],
				((BX-OX)^2 + (BY-OY)^2) < 8 % close-by, rounding errors
			 )),              % without obstacles
	can_bounce_off(OX, OY, X, Y, Para_A, Para_B, DIR, Conf),
%    writeln([Para_A, Para_B]),
	slingshotPivot(X0,Y0),
	X0_INT is round(X0),
	Y0_INT is round(Y0),
	TAP_INT is round(TAP),
	ShotAngle is round(180.0*(A_RAD / pi)).
