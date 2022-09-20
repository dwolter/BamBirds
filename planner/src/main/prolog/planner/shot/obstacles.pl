:- module(shot_obstacles, [
	shot_obstacles_and_hit/3,
	shot_obstacles/3,
	shot_obstacles/4,
	shot_obstacles/5,
	fewest_shot_obstacles/2,
	fewest_shot_obstacles_no_hill/2
	]).
:- use_module(collision_shapes).
:- use_module(planner(data)).
:- use_module(planner(ab)).
:- use_module(library(yall)).

sort_by_hit_x(REL, [_,X1,_], [_,X2,_]) :-
	compare(REL, X1, X2).

shot_obstacles_and_hit(Target, Objects_Hits, UUID) :-
	slingshotPivot(X0,Y0),
	parabola(Target,UUID, [HITX,_], _, A, B),
	DXMAX is HITX - X0, % +(0.5*W)  
	findall([O,HX,HY],
		(
	object_on_parabola(O, X0, Y0, DXMAX, A, B, HX),
	O \== Target,
	HY is (Y0 - A*(HX-X0)^2 - B*(HX-X0))
		),
				OH),
	predsort(sort_by_hit_x, OH, Objects_Hits).

:- table shot_obstacles/3, shot_obstacles/4, shot_obstacles/5.

shot_obstacles(Target, Objects, UUID) :-
	slingshotPivot(X0,_),
	parabola(Target,UUID, [HITX,_], _, A, B),
	DXMAX is HITX - X0, % +(0.5*W),
	shot_obstacles(A,B,Target,DXMAX,Objects_Hits),
	maplist([[O|_],O]>>true, Objects_Hits, Objects).


%% computes obstacles on shot given by parabola A,B with local X coordinate smaller DXMAX
%% on return, Obstacles is list of lists: [[Object, Hit-X, HitY] ...]shot_obstacles(A, B, DXMAX, Obstacles) :-
shot_obstacles(A, B, DXMAX, Obstacles) :-
	slingshotPivot(X0, Y0),
	findall([O,HX,HY],
		(
			object_on_parabola(O, X0, Y0, DXMAX, A, B, HX),
			HY is (Y0 - A*(HX-X0)^2 - B*(HX-X0))
		),
		OH),
	predsort(sort_by_hit_x, OH, Obstacles).

shot_obstacles(A, B, Target, DXMAX, Obstacles) :-
	shot_obstacles(A, B, DXMAX, AllObstacles),
	exclude([[O|_]]>>(O=Target), AllObstacles, Obstacles).

fewest_shot_obstacles(Target, Objects) :-
	findall((O,L), (
		shot_obstacles(Target, O,_), 
		length(O,L)
	), [(O1,L1)|Rest]),
	common_list:shortest_list(Rest, O1, L1, Objects).

fewest_shot_obstacles_no_hill(Target, Objects) :-
	findall((O,L), (
		shot_obstacles(Target, O,_),
		include(hill, O, []),
		length(O,L)
	), [(O1,L1)|Rest]),
	common_list:shortest_list(Rest, O1, L1, Objects).