:- module(plans_tnt, [tnt/5, plans_common:plan/2]).
:- use_module(planner(shot)).
:- use_module(planner(data)).
:- use_module(planner(ab)).
:- use_module(common).


plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"tnt", confidence:C, reasons:Reasons}) :-
	\+ whitebird(Bird),
	tnt(Bird, Target, UUID, C, Reasons),
	>(C, 0.49),
	shot_params_dict(UUID, Shot, ImpactAngle).

% tnt_(+Bird, -Target, -Confidence)
% target a TNT if there are pigs immediately affected 
tnt(Bird, Target, UUID, C, Reasons) :-
	hasMaterial(Target,tnt,_,_,_,_),
	shot_obstacles(Target, Obstacles, UUID),
	penetration(Bird, Obstacles, PE),
	PE < 0.99,
	findall(P, (pig(P), canExplode(Target, P)), ExplodedPigs),
	belongsTo(Target, Struct),
	findall(O, belongsTo(O, Struct), Damaged),
	include(pig, Damaged, AllPigsDamaged),
	subtract(AllPigsDamaged, ExplodedPigs, PigsDamaged),
	length(Damaged, NObjects),
	explosion_clears_path(Damaged, AllLaterPigs),
	append(ExplodedPigs, PigsDamaged, AllAffectedPigs),
	subtract(AllLaterPigs, AllAffectedPigs, LaterPigs),
	%	format('target=~w, Pigs=~w, PigsDamaged=~w, objects damaged=~w~n', [Target,Pigs,PigsDamaged,NObjects]),
	(
		ExplodedPigs\=[] -> C is 1.0 ;
			C is min(0.9, 0.5+0.05*NObjects)
	),
	merge_reasons([Target|ExplodedPigs], PigsDamaged, LaterPigs, Reasons).

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
