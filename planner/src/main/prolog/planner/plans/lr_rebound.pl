:- module(plans_rebound, [rebound/7, plans_common:plan_last_resort/3]).
:- use_foreign_library(lib(behind_the_corner)).
:- use_module(library(prolog_stack)).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).
:- use_module(planner(physics/projectile_motion)).


plans_common:plan_last_resort(Bird, Target, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"reboundLR", confidence:C, reasons:Reasons}) :-
	catch_with_backtrace(
		(behind_the_corner:connect ->
			(
				load_scene_representation(Handle),
				rebound(Handle, Bird, Target, ImpactAngle, C, Reasons, Shot)
			);
			resource_error(behind_the_corner)
		),
		Error,
		(
			writeln(user_error, 'Failed to validate connection to rebound library. The following error occured:'),
			print_message(error, Error),
			false
		)
	).

%--------
% Scene representation
%--------

load_scene_representation(Handle) :-
	sceneRepresentation(FileName),
	behind_the_corner:load_scene(FileName, Handle).

% Maximum iteration count dependent on edge length
maxiters(15, 15000).
maxiters(14, 25000).
maxiters(13, 30000).
maxiters(12, 40000).
maxiters(E, 50000) :- E < 12.

target_size(Target, Size) :-
	shape(Target, ball, _, _, _, [TRadius]),
	Size is TRadius * 2, !.
target_size(Target, Size) :-
	shape(Target, rect, _, _, _, [Width, Height, _]),
	Size is max(Width, Height), !.

narrower_starting_interval(Bird, Results, [LowerBound,HigherBound], Velocity) :-
	findall([Start,End,[FirstHitPointX,FirstHitPointY]], 
		(
			member(Result, Results),
			Result =.. [_,Start,End,FirstHitPointX,FirstHitPointY, _]
		),
		ConvertedResults
	),
	sort(1, <, ConvertedResults, SortedResults),
	nth0(0, SortedResults, [FirstStart,_,_]),
	last(SortedResults, [_,LastEnd,_]),
	(LastEnd < 0 -> 
		(
			LowerBound is max(-1.5, FirstStart - 0.3),
			HigherBound is LastEnd + 0.05,
			unnormalized_velocity(Bird,LastEnd,Velocity)
		);
		(
			LowerBound is FirstStart - 0.05,
			HigherBound is min(1.5, LastEnd + 0.3),
			unnormalized_velocity(Bird,FirstStart,Velocity)
		)
	).

unnormalized_velocity(Bird,Angle,Velocity) :-
	hasColor(Bird, Color),
	launch_velocity(Color,Angle,VNormalized),
	scene_scale(SlingshotScale,ScalingFactor),
	UnnormalizeSlingshotScalingFactor is sqrt(SlingshotScale),
	Velocity is (VNormalized*UnnormalizeSlingshotScalingFactor) / ScalingFactor.

rebound(FileHandle, Bird, Target, ImpactAngle, C, Reasons, Shot) :-
	target_size(Target, TSize),
	abType(Target,TType),
	slingshotPivot(SX,SY),
	shape(Target, _, TX, TY, _, _),
	unnormalized_velocity(Bird,0.7853,VAt45),
	member(EdgeLength, [9,8]),
	maxiters(EdgeLength, MaxIter),
	% Execute simulation
	behind_the_corner:simulate(FileHandle, _{
        start: [SX,SY],
        target: [TX,TY],
        target_type:TType,
        target_size:TSize,
        angle_range:[-1.5,1.5],
        velocity: VAt45,
        gravity: 1,
        edge_length: EdgeLength,
        maxiter: MaxIter
    }, CoarseResults),
	% Only allow one solution for now
	!,
	% Second iteration of the simulation
	narrower_starting_interval(Bird, CoarseResults,AngleInterval, V0),
	ReducedMaxIter is MaxIter / 2,
	behind_the_corner:simulate(FileHandle, _{
        start: [SX,SY],
        target: [TX,TY],
        target_type:TType,
        target_size:TSize,
        angle_range:AngleInterval,
        velocity: V0,
        gravity: 1,
        edge_length: 7,
        maxiter: ReducedMaxIter
    }, Results),
	(Results \= [] ->
		ResultsForShots = Results;
		ResultsForShots = CoarseResults
	),
	member(Result, ResultsForShots),
	% write_ln(Result),
	createShot(Bird, Result, C, ImpactAngle, Shot),
	Reasons = [Target].

createShot(Bird, Result, C, ImpactAngle, shot{sling_x:X0_INT, sling_y:Y0_INT, drag_x:RX, drag_y:RY, target_x:FirstHitPointX, target_y:FirstHitPointY, tap_time:0}) :-
	Result =.. [Functor,AngleRangeStart,AngleRangeEnd,FirstHitPointX,FirstHitPointY, Distance],
	slingshotPivot(X0, Y0),
	(Functor == btc_result ->
		% Normal plans should still be preferred if possible
		C = 0.9;
		% If result is defined as close, the confidence is defined by how close it actually got
		C is 0.7 - Distance
	),
	shots_at_point(Bird, FirstHitPointX, FirstHitPointY, Shots),
	member([_,_,Angle,A,B,_,_,RX,RY], Shots),
	AngleRangeStartWithTolerance is AngleRangeStart - 0.1,
	AngleRangeEndWithTolerance is AngleRangeEnd + 0.1,
	bounded_number(AngleRangeStartWithTolerance, AngleRangeEndWithTolerance, Angle),
	angleAtX(A,B,FirstHitPointX,ImpactAngle),
	X0_INT is round(X0),
	Y0_INT is round(Y0).