:- module(plans_lr_yellow_shot, [yellow_shot/4, plans_common:plan_last_resort/2, plans_common:plan_last_resort/3]).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(physics)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).

%%%
%%% yellow bird targetting
%%%

plans_common:plan_last_resort(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:42, strategy:"yellowShotLR", confidence:C, reasons:Reasons}) :-
	ab_objects:yellowbird(Bird),
	yellow_shot(Target, C, Reasons, Shot).
plans_common:plan_last_resort(Bird, Target, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:42, strategy:"yellowShotLR", confidence:C, reasons:Reasons}) :-
	ab_objects:yellowbird(Bird),
	yellow_shot(Target, C, Reasons, Shot).


/* our magic yellow bird dropoff factor*/
yellowDropFactor(0.00012).


getRelativeTarget(TOX, TOY, TRX, TRY) :-
	slingshotPivot(X0, Y0),
	scene_scale(_, S),
	TRX is (( TOX - X0 ) / S),    % relative target point
	TRY is (( Y0 - TOY ) / S).    % needs to reversed 

getActualPostition( TRX, TRY, TOX, TOY):-
	slingshotPivot(X0, Y0),
	scene_scale(_, S),
	TOX is ( TRX * S + X0 ),
	TOY is ( (-1) * ( TRY * S - Y0 ) ).

	
derivationHitsTarget(A, B, X, TY, TX, Slope, T, P):-
	Slope is 2 * A * X + B ,
	T is TY - ( Slope * TX ),
	P is A * X * X + B * X,
	yellowDropFactor(YDF),
	Y is YDF * X * X + Slope * X + T,
	abs( P - Y ) < 1 .

findShotInRange(Xmin , Xmax , A, B, TY , TX, X, Slope ,T, P) :-
	( derivationHitsTarget(A, B, Xmin, TY, TX, Slope ,T, P))
	-> X is Xmin, !
	;
	Xmin < Xmax,
	NewX is Xmin + 1,
	findShotInRange(NewX, Xmax, A, B, TY, TX, X, Slope, T, P).

verifyShotInRange(Xmin , Xmax , A, B, TRY , TRX, TAPX, Slope ,T, TAPY, E) :-
	findShotInRange(Xmin , Xmax , A, B, TRY , TRX, TAPX, Slope ,T, TAPY),
	slingshotPivot(X0, Y0),
	getActualPostition( TAPX, TAPY, TOX, TOY),
	yellowDropFactor(YDF),
	\+object_on_parabola(_, X0, Y0,TAPX, -1*A , -1*B , _),			% no object on flight to TapPoint
	findall(Object,
		object_on_parabola(Object, TOX , TOY ,TRX-TAPX, (-1)* YDF, -1*Slope , _),
		Blocker),
	hasColor(Bird, yellow), % get some yellow bird to use with penetration/3
	penetration(Bird, Blocker, E),
	E < 0.9. % absorb no less than 90% of energy
	
%findTapPoint(Ty, Tx, AngleMax, AngleMin, Angle, A, B, TapX, TapY, Slope ,T):-
%	angleToRadiant(AngleMin , Radiant),
%	parabola_for_actual_angle(yellow, Radiant, A, B),
%	(verifyShotInRange(0, Tx, A, B, Ty, Tx, TapX, Slope, T, TapY) )
%		-> Angle is AngleMin, !
%		;
%		AngleMin < AngleMax,
%		NewAngle = AngleMin + 1,
%		findTapPoint(Ty, Tx, AngleMax, NewAngle, Angle, A, B, TapX, TapY, Slope, T).

findTapPoint_ALT(Ty, Tx, DeltaAngle, Angle, A, B, TapX, TapY, Slope, T, Energy):-
	angleToRadiant(45-DeltaAngle , Radiant),
	parabola_for_actual_angle(yellow, Radiant, A, B),
	(verifyShotInRange(0, Tx, A, B, Ty, Tx, TapX, Slope, T, TapY, Energy) )
		-> Angle is 45-DeltaAngle, !
		;
		angleToRadiant(45+DeltaAngle , Radiant),
		parabola_for_actual_angle(yellow, Radiant, A, B),
		(verifyShotInRange(0, Tx, A, B, Ty, Tx, TapX, Slope, T, TapY, Energy) )
			-> (
				Angle is 45+DeltaAngle, !
				)
			;
			DeltaAngle < 45,
			NewDeltaAngle = DeltaAngle + 1,
			findTapPoint_ALT(Ty, Tx, NewDeltaAngle, Angle, A, B, TapX, TapY, Slope, T, Energy).

yellowShotAtTarget(TRX, TRY ,RX ,RY, A,B , TAP_INT, Energy) :-
	% our old solution
	%findTapPoint(TRY,TRX,90,0,Angle,_,_,TapX,_,_,_) 
	% new alternative solution
	findTapPoint_ALT(TRY, TRX, 0, Angle, A, B, TapX, _, _, _, Energy),
	% this angle has to be a radiant  
	angleToRadiant(Angle , Rad),
	% get the time of flight till TapPoint
	launch_velocity(yellow, Rad, V),time_of_flight(V, Rad, TapX, TOF),
	angle_to_release_point(yellow, Rad, RX, RY),
	% do we really need this 0,95 TEST
	TAP_INT is round(TOF).

yellow_shot(Target, C, [destroy(Target)], shot{sling_x:X0_INT, sling_y:Y0_INT, drag_x:RX, drag_y:RY, target_x:TOX, target_y:TOY, tap_time:TAP_INT, uuid:UUID}) :-
	% no need if there is also a parabola that reaches the target
	\+isHittable(Target, _),
	%shape(Target, _, TOX, TOY, _, _),
	hasMaterial(Target, _, TOX, TOY, _, _),
	getRelativeTarget(TOX, TOY, TRX, TRY), % relative Target Point
	yellowShotAtTarget(TRX, TRY ,RX ,RY, A, B, TAP_INT, Energy),
	assert_parabola_for_shot(Target, TOX, TOY, A, B, UUID),
	C is (1 - 0.1*Energy) * 0.5,
	slingshotPivot(X0, Y0), % Schuss in Mausklick umrechnen
	X0_INT is round(X0),
	Y0_INT is round(Y0).
