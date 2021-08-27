:- module(plans_bunker, [bunker/5, plans_common:plan/2]).
:- use_module(common).
:- use_module(planner(ab/relations)).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot/hittable)).
:- use_module(planner(shot/simple)).
:- use_module(planner(shot/obstacles)).
:- use_module(planner(physics/impact)).
:- use_module(planner(data)).


plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"bunker", confidence:C, reasons:Pigs}) :-
	bunker(Bird, Target, ImpactAngle, C, Pigs),
	>(C,0.49),
	shot_params_dict(ImpactAngle, Shot).

%% penetration shot that requires multiple birds for digging into bunker
bunker(Bird, Target, Angle, C, [Pig]) :-
	pig(Pig),
	\+isHittable(Pig,_),
	shot_obstacles(Pig, Obstacles, Angle),
	findall(M, (member(O, Obstacles), hasMaterial(O, M)), Materials),
	\+member(hill, Materials),
	penetration(Bird, Obstacles, PE),
	num_of_birds_remaining(NBirds),
	PE < NBirds,
	num_of_pigs_remaining(NPigs),
	nth0(0, Obstacles, Target),
	% Higher Confidence if only small number of birds required and if Less Pigs remain
	C is min(1, 1 - (PE / NBirds) + (1 / NPigs)).

bunker(Bird, Target, Angle, C, AllPigs) :-
	%    protects(Struct, Pig),
	pig(Pig,PX,PY,_,_),
	\+isHittable(Pig,_),
	structure(Struct),
	%    isAnchorPointFor(Target, Struct),
	belongsTo(Target,Struct),
	isHittable(Target, Angle),
	structure_bounding_box(Struct, X0, Y0, X1, Y1),
	point_contained(PX, PY, X0, Y0, X1, Y1),
	hasMaterial(Target, M, _, _, _, _),
	dominoImpact(Bird, M, I),
	findall(P, (pig(P,PX,PY,_,_), point_contained(PX,PY, X0, Y0, X1, Y1)), AllPigs),
	RoundedAngle is round(Angle),
	(between(-30,45,RoundedAngle) -> P is 0.0; P is 1.2), % prefer low shots on left half
	(hasOrientation(Target, horizontal), on_groundlike(Target) -> D is 0.4; D is 0.0), % avoid horizontal plans
	(hasOrientation(Target, horizontal), \+on_groundlike(Target) -> D2 is -0.2; D2 is 0.0),
	C is min(1.0, 0.2+(0.05 * I)-P-D-D2).
