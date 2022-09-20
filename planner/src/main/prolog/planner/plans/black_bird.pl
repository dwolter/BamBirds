:- module(plans_black_bird, [blackBird/5, plans_common:plan/2]).
:- use_module(planner(ab)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).


plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"blackBird", confidence:C, reasons:Reasons}) :-
	blackbird(Bird),
	blackBird(Bird, Target, UUID, C, Reasons),
	>(C, 0.49),
	shot_params_dict(UUID, Shot, ImpactAngle).

%plan for black birds, work in progress! isHittable not recognized in situations, has to be looked at!

% Explode black bird in a structure with pig(s)
% blackBird_(+Bird, -Target, -Confidence, -Pigs)
blackBird(_, Target, UUID, Conf, Reasons) :-
	object(Target),
	isHittable(Target,_),
	\+hill(Target),
	\+pig(Target),
	belongsTo(Target,Structure1),
	pigs_in_struct(Structure1, P, Pigs),
	merge_reasons([], Pigs, [], Reasons),
	P>0,
	flachschuss(Target, UUID),
	Conf is 0.7.

% Explode black bird in a structure that (might) collapse a strucute with pig(s)
blackBird(_, Target, UUID, Conf, Reasons) :-
	object(Target),
	isHittable(Target,_),
	\+hill(Target),
	\+pig(Target),
	structure(Structure),
	structure(Structure2),
	belongsTo(Target,Structure2),
	collapsesInDirection(Structure,Structure2,away),
	collapsesInDirection(Structure2,Structure,towards),
	pigs_in_struct(Structure2, P, Pigs),
	P>0,
	merge_reasons([], Pigs, [], Reasons),
	flachschuss(Target,UUID),
	Conf is 0.5.

% shoot close to some pig(s) that will be killed by the explosion
blackBird(_, Target, UUID, Conf, Reasons) :-
	object(Target),
	isHittable(Target, UUID),
	\+hill(Target),
	\+pig(Target),
	shape(Target, _, TX, TY, _, _),
	pigs_in_range(TX, TY, Conf, Pigs),
	merge_reasons(Pigs, [], [], Reasons).

%% rule to bomb through hills, shooting at the hill
%% NB: special case not handled by previous methods, since
%%     hills are no targets; hence, we shoot directly at the pig
%% FIXME: THIS REALLY SHOULD BE HANDLED BY PENETRATION SHOTS!
blackBird(_, Pig, UUID, Conf, Reasons) :-
	pig(Pig),
	\+ isHittable(Pig, _),
	parabola(Pig, UUID, [HX,_], _, A, B), % check hitting at hill?!
	shot_obstacles(A,B,Pig,HX,[[_,FirstHitX,FirstHitY] | _]),
	pigs_in_range(FirstHitX,FirstHitY, Conf, Pigs),
	merge_reasons(Pigs, [], [], Reasons).
