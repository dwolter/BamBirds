:- module(plans_airbomb, [airbomb/5, plans_common:plan/2]).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).

%%%
%%% white birds' airbombs
%%%

plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:42, strategy:"airbomb", confidence:C, reasons:Pigs}) :-
	hasColor(Bird,white),
	airbomb(Bird, Target, Shot, C, Pigs).

down_order(=,[O,_],[O,_]).
down_order(<,[_,Y1],[_,Y2]) :- Y1<Y2.
down_order(>,[_,Y1],[_,Y2]) :- Y1>Y2.

pigs_on_drop(PX, YUP, YDOWN, Pigs) :-
	%% all objects on path
	findall([O,HITY], (shape(O,_,_,_,_,_), object_on_vline(O,PX,YUP,YDOWN,HITY)), ObsHits),
	predsort(down_order, ObsHits, Objects),
	!,
	\+((member([H,_], Objects), hill(H))), % no hill inbetween
	( Objects=[] ->
		findall(P, (pig(P), shape(P,_,CPX,CPY,_,_), sqrt((CPX-PX)^2 + (CPY-YDOWN)^2)<30), Pigs)
	;
	(Objects=[[_,Y1]|_], findall(P, (pig(P), shape(P,_,CPX,CPY,_,_), sqrt((CPX-PX)^2 + (CPY-Y1)^2)<30), Pigs))).



white_second_damage(X,Y, CNT) :-
	LX2 is X+Y,
%    writeln([[X,Y], [LX2,0]]),
	findall(O, (shape(O, ST, CX, CY, _, SD), line_crosses_bb(O, X, Y, LX2, 0), line_crosses_shape(ST, CX, CY, SD, X, Y, LX2, 0, _, _)), HITS),
%    writeln(HITS),
	length(HITS, CNT).

bombtarget(Bird, PX, PY, Y, [X0, Y0, RX, RY, Y, TAP], C, Pigs) :-
	Y >= 0,
	slingshotPivot(X0,Y0),
	pigs_on_drop(PX, Y, PY, Pigs),
	!,
	shots_at_point(Bird, PX, Y, SHOTS),
	(
		(member([_,_,A_RAD,_,_,[],EARLYTAP,RX,RY], SHOTS), !, 
		white_second_damage(PX,Y, CNT),
		(A_RAD > 0.78 ->
				TAP is EARLYTAP;
				TAP is (EARLYTAP*1.2)),
		C is 1.0+0.01*CNT)
		;
		(YABOVE is Y-20, bombtarget(Bird, PX, PY, YABOVE, [X0, Y0, RX, RY, TAP], C, Pigs))
	).


%% air bombs
airbomb(Bird, Target, shot{sling_x:X0_INT, sling_y:Y0_INT, drag_x:RX, drag_y:RY, target_x:PX, target_y:TY, tap_time:TAP_INT}, C2, Pigs) :-
	is_goal(Target, Value),
	shape(Target,_,PX,PY,_,_),
	SY is PY-20,
	bombtarget(Bird, PX, PY, SY, [X0,Y0,RX,RY,TY,TAP], C,Pigs),
	C2 is C*Value,
	X0_INT is round(X0),
	Y0_INT is round(Y0),
	TAP_INT is round(TAP).