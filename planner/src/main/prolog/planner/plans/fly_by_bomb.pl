:- module(plans_fly_by_bomb, [flybyshot/8, plans_common:plan/2]).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(ab)).
:- use_module(planner(data)).
:- use_module(planner(contrib/between_with_step)).

% fly-by bombing with black bird
plans_common:plan(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:42, strategy:"flyByBomb", confidence:C, reasons:Reasons}) :-
	blackbird(Bird),
	pig(Target),
	\+ isHittable(Target, _),
	shape(Target, _, PX, PY, _, _),
	between(10,65,10,Delta), 
	flybyshot(Bird, Target, PX, PY, Delta, Shot, C, Reasons).
	
%% make black birds explode close to unreachable targets
flybyshot(Bird, Target, PX, PY, Delta, shot{sling_x:X0_INT, sling_y:Y0_INT, drag_x:RX, drag_y:RY, target_x:X, target_y:Y, tap_time:TAP_INT, uuid:UUID}, C, Reasons) :-
	PY1 is (PY-Delta),
	PY2 is (PY+Delta),
	PX1 is (PX+Delta),
	PX2 is (PX-Delta),
	PTS = [[PX, PY1], [PX, PY2], [PX1, PY], [PX2, PY]],
	member([X,Y], PTS),
	shots_at_point(Bird, X, Y, SHOTS),
	member([TX,TY,_, A, B,[],EARLYTAP,RX,RY], SHOTS),
	slingshotPivot(X0, Y0),
	X0_INT is round(X0),
	Y0_INT is round(Y0),
	pigs_in_range(X, Y, C, KilledPigs),
	\+KilledPigs= [],
	assert_parabola_for_shot(Target, TX, TY, A, B, UUID),
	%%% FIXME check time to drop
	TAP_INT is round(EARLYTAP * 1.16),
	merge_reasons(KilledPigs, [], [], Reasons).
