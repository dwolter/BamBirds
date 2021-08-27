:- module(plans_heavy_object, [heavyObject/5, plans_common:plan/2]).
:- use_module(planner(ab)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).


plans_common:plan(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"heavyObject", confidence:C, reasons:Pigs}) :-
	heavyObject(Bird, Target, ImpactAngle, C, Pigs),
	>(C, 0.49),
	shot_params_dict(ImpactAngle, Shot).

% increasePotential_(+Potential, -NewPotential)
increasePotential(Potential, NewPotential) :-
	NewPotential is Potential + 10.

% pigNotConsideredYet_(+AnotherPig, +ListOfPigs)
pigNotConsideredYet(_, []) :-
	true.
pigNotConsideredYet(AnotherPig, [Pig | Pigs]) :-
	AnotherPig \= Pig,
	pigNotConsideredYet(AnotherPig, Pigs).

% calcHeavyObjectC_(+Ball, +ListOfPigs, +Potential, -Confidence)
calcHeavyObjectC(Ball, Pigs, Potential, C, _) :-
	pig(AnotherPig),
	isOver(Ball, AnotherPig),
	pigNotConsideredYet(AnotherPig, Pigs),
	append([AnotherPig], Pigs, NewListOfPigs),
	increasePotential(Potential, NewPotential),
	<(NewPotential, 100),
	!,
	calcHeavyObjectC(Ball, NewListOfPigs, NewPotential, C, _).
calcHeavyObjectC(_Ball, _Target, Potential, C, _) :-
	C is Potential / 100.

% Tries to find a ball that is above pigs and target the object that is most
% likely to support said ball. Potential targets are: the object the ball is
% on, the object that supports the ball or the ball itself.

supports_heavy_object(Object,HeavyObject) :-
	supports(_,_), !,
	supports(Object,HeavyObject).
supports_heavy_object(Object,HeavyObject) :-
	\+ supports(_,_),
	(isLeft(Object,HeavyObject) ; isRight(Object,HeavyObject)).

% heavyObject_(+Bird, -Target, -Confidence)
heavyObject(_, Target, Angle, C, []) :-
	object(Ball),
	hasForm(Ball, ball),
	pig(Pig),
	((isOver(Ball, Pig) ; (belongsTo(Ball,Struct), worth(Struct))), !),
	isOn(Ball, Target), % Target: Object the ball is on, object is hittable
	\+hill(Target),
	isHittable(Target,Angle),
	calcHeavyObjectC(Ball, [Pig], 65, C, _).

% Reminder: sort-of duplicate deleted.

heavyObject(_, Target, Angle, C, []) :-
	object(Ball),
	hasForm(Ball, ball),
	pig(Pig),
	isOver(Ball, Pig),
	supports_heavy_object(Target, Ball), % Target: Object that supports the ball
	\+hill(Target),
	isHittable(Target, Angle),
	calcHeavyObjectC(Ball, [Pig], 50, C, _).

heavyObject(_, Target, Angle, C, [Pig]) :-
	hasForm(Target, ball),
	isHittable(Target, Angle),
	pig(Pig),
	isOver(Ball, Pig), %% FIXME: add hills and slopes..
	calcHeavyObjectC(Ball, [Pig], 40, C, _).

% WIP / FIXME / TODO : Heavy Object on Slope

heavyObject(_, Ball, Angle, 0.5, []) :-
	hasForm(Ball, ball),
	isHittable(Ball, _),
	flachschuss(Ball, Angle).

heavyObject(_, Target, Angle, C, []) :-
	object(Ball),
	hasForm(Ball, ball),
	isOn(Ball,Hill),
	hill(Hill),
	is_goal(Victim, Importance),
	(Importance > 0.6),
	isOver(Ball, Victim),
	supports_heavy_object(Target, Ball), % Target: Object that supports the ball
	\+hill(Target),
	isHittable(Target, Angle),
	calcHeavyObjectC(Ball, [Victim], 50, C, _).