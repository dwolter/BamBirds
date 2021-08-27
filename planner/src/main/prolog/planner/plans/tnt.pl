:- module(plans_tnt, [tnt/5, plans_common:plan/2]).
:- use_module(planner(shot)).
:- use_module(planner(data)).
:- use_module(planner(ab)).
:- use_module(common).


plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"tnt", confidence:C, reasons:Pigs}) :-
	tnt(Bird, Target, ImpactAngle, C, Pigs),
	>(C, 0.49),
	shot_params_dict(ImpactAngle, Shot).

% tnt_(+Bird, -Target, -Confidence)
% target a TNT if there are pigs immediately affected 
tnt(Bird, Target, Angle, C, Pigs):-
    hasMaterial(Target,tnt,_,_,_,_),
    shot_obstacles(Target, Obstacles, Angle),
    penetration(Bird, Obstacles, PE),
    PE < 0.99,
    findall(P, (pig(P), canExplode(Target, P)), ExplodedPigs),
    belongsTo(Target, Struct),
    findall(O, belongsTo(O, Struct), Damaged),
    aggregate_all(count, (member(P, Damaged), pig(P)), PigsDamaged),
    length(Damaged, NObjects),
	explosion_clears_path(Damaged, LaterPigs),
	union(ExplodedPigs, LaterPigs, Pigs),
    %	format('target=~w, Pigs=~w, PigsDamaged=~w, objects damaged=~w~n', [Target,Pigs,PigsDamaged,NObjects]),
    ((ExplodedPigs=[], PigsDamaged==0) -> 
	 ( LaterPigs\=[] -> C is 0.8; C is 0.49)
    ;
     (
		ExplodedPigs\=[] -> C is 1.0 ;
			    C is min(0.9, 0.5+0.05*NObjects)
     )
    ).

explosion_clears_path(Damaged, Pigs) :-
	findall(P, 
		(
			pig(P),
			\+ isHittable(P, _),
			once((
				shot_obstacles(P, Obstacles, _),
				\+intersection(Obstacles, Damaged, [])
			))
		), 
		Pigs
	).
