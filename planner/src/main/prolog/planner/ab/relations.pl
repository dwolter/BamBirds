:- module(ab_relations, [
	isOnGroundOrHill/1,
	on_groundlike/1,
	in_vertical_corridor/5,
	isLowInStructure/1,
	isInFront/2,
	isProperStructure/1,
	pigs_in_struct/3,
	in_slingshot/1
]).
:- use_module(planner(data)).
:- use_module(objects).
:- use_module(planner(geometric/boxes)).


isOnGroundOrHill(Object) :- %% FIXME: objects can be on(Obj,ground). (use on_groundlike below!!)
	(hill(Hill),
	isOn(Object, Hill));
	\+isOn(Object, _).

on_groundlike(Object) :-
	isOn(Object,ground).

on_groundlike(Object) :-
	isOn(Object, Support),
	hill(Support).

in_vertical_corridor(Obj, XMin, Xmax, Ymin, Ymax) :-
	hasMaterial(Obj,_,X,Y,W,H),
	X2 is X + W,
	Y2 is Y + H,
	boxes_overlap(XMin, Ymin, Xmax, Ymax, X, Y, X2, Y2).

isLowInStructure(Object) :-
	on_groundlike(AnotherObject),
	isOn(Object, AnotherObject).

% This function will check if a given object is in front of another (Object1 left or Object2 right)
% isInFront_(+Target, +Object)
isInFront(Object1, Object2) :-
	isLeft(Object1, Object2);
	isRight(Object2, Object1).

% Proper structure: a structure with at least three objects belonging to it
% isProperStructure_(+Structure)
isProperStructure(Structure) :-
	aggregate_all(count, belongsTo(_,Structure), N),
	N >= 3.

pigs_in_struct(Structure, N, Pigs) :-
	%    bagof(P, (pig(P), belongsTo(P, Structure)), Pigs),
	%    length(Pigs,N), !.
	findall(P, (pig(P), belongsTo(P, Structure)), Pigs),
	length(Pigs,N).
	
%pigs_in_struct(_, 0, []).

/**
 * in_slingshot(-Bird) is semidet
 * 
 * Determines if a Bird is in the slingshot.
 */
in_slingshot(Bird) :-
	birdOrder(Bird, 0).

%% TODO: WIP Placeholder for potential Slope detection
%isOnSlope(Object, Direction) :-
%	hill(Hill),
%	isOn(Object, Hill),
