:- module(plans_black_bird, [blackBird/5, plans_common:plan/2]).
:- use_module(planner(ab)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).


plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"blackBird", confidence:C, reasons:Pigs}) :-
	blackBird(Bird, Target, ImpactAngle, C, Pigs),
	>(C, 0.49),
	shot_params_dict(ImpactAngle, Shot).

%plan for black birds, work in progress! isHittable not recognized in situations, has to be looked at!

% blackBird_(+Bird, -Target, -Confidence, -Pigs)
blackBird(Bird, Target, Angle, Conf, Pigs) :-
	blackbird(Bird),
	object(Target),
	isHittable(Target,_),
	\+hill(Target),
	\+pig(Target),
	%structure(Structure1),
	belongsTo(Target,Structure1),
	collapsesInDirection(Structure1,_Structure2,away),
	pigs_in_struct(Structure1, P, Pigs),
	P>0,
	flachschuss(Target,Angle),
	Conf is min(1.0, 0.7+P).

blackBird(Bird, Target, Angle, Conf, Pigs) :-
	blackbird(Bird),
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
	flachschuss(Target,Angle),
	Conf is min(1.0, 0.75+P).

% shoot close to some pig(s) that will be killed by the explosion
blackBird(Bird, Target, Angle, Conf, Pigs) :-
	blackbird(Bird),
	object(Target),
	isHittable(Target, Angle),
	\+hill(Target),
	\+pig(Target),
	shape(Target, _, TX, TY, _, _),
	pigs_in_range(TX, TY, Conf, Pigs).

%% rule to bomb through hills, shooting at the hill
%% NB: special case not handled by previous methods, since
%%     hills are no targets; hence, we shoot directly at the pig
%% FIXME: THIS REALLY SHOULD BE HANDLED BY PENETRATION SHOTS!
blackBird(Bird, Pig, Angle, Conf, Pigs) :-
	blackbird(Bird),
	pig(Pig),
	shot_obstacles_and_hit(Pig, [[_, HX, HY] | _], Angle), % check hitting at hill?!
	pigs_in_range(HX, HY, Conf, Pigs).