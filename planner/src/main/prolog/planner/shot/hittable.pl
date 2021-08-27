:- module(shot_hittable, [
	all_hittable/1,
	retract_unhittable/0,
	assert_hittable/0,
	assert_parabolas_and_hittable/0
	]).
:- use_module(obstacles).
:- use_module(collision_shapes).
:- use_module(planner(data)).
:- use_module(planner(ab/relations)).
:- use_module(planner(physics/projectile_motion)).
:- use_module(planner(geometric/common)).
:- use_module(planner(geometric/polygon)).

all_hittable([]).
all_hittable([T|Ts]) :-
	isHittable(T,_), !,
	all_hittable(Ts).

retract_unhittable :-
	forall(isHittable(Object, Angle),
		(shot_obstacles(Object, [], Angle) ->
			true
			;
			data:retract(isHittable(Object, Angle)))
	).

assert_hittable :-
	forall(shot_obstacles(Object, [], Angle),
		data:assertz(isHittable(Object, Angle))).

assert_parabolas_and_hittable :-
	in_slingshot(Bird),
	forall((
			object(Object), 
			hit_point(Object, [HitX, HitY]), 
			parabola_for_target_point(Bird, [HitX, HitY], ImpactAngle, A, B)
		),
		(
			((
				object_on_parabola(Object, HitX, A, B, ActualHitX),
				parabola_y_value(A,B,ActualHitX,ActualHitY)
			); 
				[ActualHitX,ActualHitY] = [HitX, HitY]
			),
			data:assertz(parabola(Object, [ActualHitX,ActualHitY], ImpactAngle, A, B)),
			(shot_obstacles(Object, [], ImpactAngle)->(
				data:assertz(isHittable(Object, ImpactAngle))
				);
				true
			)
		)
	).

hit_point(Object, HitPoint) :-
	col_shape(Object, Shape, X, Y, Area, Spec),
	hit_point(Shape, X, Y, Area, Spec, HitPoint).
hit_point(Shape, X, Y, _, _, [X, Y]) :-
	\+ Shape == poly.
hit_point(ball, X, Y, Area, [R], HitPoint) :-
	( Area > 150 ->
		(
			between(0, 7, K),
			TX is X + ((R * 0.6) * cos(K * (pi/4))),
			TY is Y + ((R * 0.6) * sin(K * (pi/4))),
			HitPoint = [TX, TY]
		);
		false
	).
	
hit_point(rect, X, Y, Area, [W, H, A], HitPoint) :-
	Area > 200,
	XRa is (0.5*H),
	YRa is (0.5*W),
	rot_shift(A, X, Y, [[-XRa,-YRa], [-XRa,YRa], [XRa,YRa], [XRa,-YRa]], Points),
	member(HitPoint, Points).
hit_point(poly, _, _, _Area, [_NPoints | Points], HitPoint) :-
	% TODO: Generate hitpoints on edges in regular distances 
	% between(0, NPoints-1, Index),
	% PointA is nth0(Points, Index),
	% PointB is nth0(Points, (Index+1) mod NPoints),
	% DistanceToNextPoint is distance(PointA, PointB),
	% ( DistanceToNextPoint > 50 ->
	% 	between();
	% 	HitPoint = PointA
	% 	).
	member(HitPoint, Points).

% SHOT==[Pivot_x, Pivot_y, Angle, Hit_X, Hit_Y, [List-of-Obstacles], Tap, Drag_x,Drag_y]
