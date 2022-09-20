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


plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"bunker", confidence:C, reasons:Reasons}) :-
	\+ whitebird(Bird),
	bunker(Bird, Target, UUID, C, Reasons),
	shot_params_dict(UUID, Shot, ImpactAngle).

%% penetration shot that requires multiple birds for digging into bunker
bunker(Bird, Target, UUID, C, Reasons) :-
	pig(Pig),
	\+isHittable(Pig,_),
	shot_obstacles(Pig, Obstacles, UUID),
	\+member(hasMaterial(_, hill), Obstacles),
	penetration(Bird, Obstacles, PE),
	num_of_birds_remaining(NBirds),
	nth0(0, Obstacles, Target),
	% Confidence depending on penetration and amount of birds left
	C is min(1, (0.5 + 0.5 * NBirds) / PE),
	C > 0.1,
	merge_reasons([], [], [Pig], Reasons).

bunker(Bird, Target, UUID, C, Reasons) :-
	%    protects(Struct, Pig),
	pig(Pig,PX,PY,_,_),
	\+isHittable(Pig,_),
	structure(Struct),
	%    isAnchorPointFor(Target, Struct),
	belongsTo(Target,Struct),
	isHittable(Target, UUID),
	structure_bounding_box(Struct, X0, Y0, X1, Y1),
	point_contained(PX, PY, X0, Y0, X1, Y1),
	hasMaterial(Target, M, _, _, _, _),
	dominoImpact(Bird, M, I),
	findall(P, (pig(P,P1X,P1Y,_,_), point_contained(P1X,P1Y, X0, Y0, X1, Y1)), AllPigs),
	aggregate(sum(XDist), (member(P,AllPigs), pig(P,P1X,_,_,_), XDist is abs(P1X - PX)), XDistancesSummed),
	length(AllPigs, LAllPigs),
	AverageXDist is XDistancesSummed / LAllPigs,
	parabola(Target,UUID,_,Angle,_,_),
	RoundedAngle is round(Angle),
	(between(-30,45,RoundedAngle) -> P is 0.0; P is 0.5), % prefer low shots on left half
	(hasOrientation(Target, horizontal), on_groundlike(Target) -> D is 0.4; D is 0.0), % avoid horizontal plans
	(hasOrientation(Target, horizontal), \+on_groundlike(Target) -> D2 is -0.2; D2 is 0.0),
	C is min(1.0, 0.2+(0.05 * I)-P-D-D2 - 0.004 * AverageXDist),
	merge_reasons([], [], AllPigs, Reasons).
