:- module(shot_obstacles, [
	shot_obstacles_and_hit/3,
	shot_obstacles/3,
	shot_obstacles/4,
	shot_obstacles/5
	]).
:- use_module(collision_shapes).
:- use_module(planner(data)).

sort_by_hit_x(REL, [_,X1,_], [_,X2,_]) :-
	compare(REL, X1, X2).

shot_obstacles_and_hit(Target, Objects_Hits, Angle) :-
	slingshotPivot(X0,Y0),
	parabola(Target,_, Angle, A, B),
	(hasMaterial(Target,_,TX,_,_,_) ; hill(Target, TX,_,_,_)),
	DXMAX is TX - X0, % +(0.5*W)   FIXME: DXMAX MUSS ANHAND DES AUFTRITTPUNKTES BESTIMMT WERDEN !!!
	findall([O,HX,HY],
		(
	object_on_parabola(O, X0, Y0, DXMAX, A, B, HX),
	O \== Target,
	HY is (Y0 - A*(HX-X0)^2 - B*(HX-X0))
		),
				OH),
	predsort(sort_by_hit_x, OH, Objects_Hits).

remove_hit_point([O,_,_], OS, [O|OS]).

:- table shot_obstacles/3, shot_obstacles/4, shot_obstacles/5.

shot_obstacles(Target, Objects, Angle) :-
	slingshotPivot(X0,Y0),
	parabola(Target,[HITX,_], Angle, A, B),
	DXMAX is HITX - X0, % +(0.5*W),
	findall([O,HX,HX], % dummy HX
		(
			object_on_parabola(O, X0, Y0, DXMAX, A, B, HX),
			O \== Target
		),
		OH),
	predsort(sort_by_hit_x, OH, Objects_Hits),
	foldl(remove_hit_point, Objects_Hits, [], Objects).

/**
 * This method calculates if there are Obstacles on the way of any parabola towards the object at its point X Y
 *
 * In contrary to shot_obstacles/3 this method does not allow the object itself to be in the way
 */
% shot_obstacles(Target, X, Y, Objects, Angle) :-
%     slingshotPivot(X0,Y0),
%     parabola(Target, X, Y, Angle, A, B),
%     DXMAX is X - X0, % +(0.5*W),
%     findall([O,HX,HX], % dummy HX
% 	    (
% 		object_on_parabola(O, X0, Y0, DXMAX, A, B, HX),
% 		(O == Target ->
%             HX < X-20;
%             true)
% 	    ),
% 	    OH),
%     predsort(sort_by_hit_x, OH, Objects_Hits),
%     foldl(remove_hit_point, Objects_Hits, [], Objects).


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
	slingshotPivot(X0, Y0),
	findall([O,HX,HY],
		(
			object_on_parabola(O, X0, Y0, DXMAX, A, B, HX),
			O \= Target,
			HY is (Y0 - A*(HX-X0)^2 - B*(HX-X0))
		),
		OH),
	predsort(sort_by_hit_x, OH, Obstacles).


fewest_shot_obstacles(Target, Objects) :-
	findall((O,L), (shot_obstacles(Target, O,_), length(O,L)) , [(O1,L1)|Rest]),
	shortest_list(Rest, O1, L1, Objects).