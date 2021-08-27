:- module(data, [
		isHittable/2,
		belongsTo/2,
		isAnchorPointFor/2,
		isCollapsable/1,
		isHittable/2,
		isAnchorPointFor/2,
		isCollapsable/1,
		isBelow/2,
		isLeft/2,
		isOn/2,
		isOver/2,
		isRight/2,
		isTower/1,
		belongsTo/2,
		bird/1,
		birdOrder/2,
		canCollapse/2,
		canExplode/2,
		collapsesInDirection/3,
		ground_plane/1,
		hasColor/2,
		hasForm/2,
		hasOrientation/2,
		hasSize/2,
		hill/5,
		parabola/5,
		pig/5,
		protects/2,
		scene_scale/2,
		shape/6,
		situation_name/1,
		slingshotPivot/2,
		structure/1,
		supports/2,
		hasMaterial/6,
		sceneRepresentation/1,
		object/1,
		purge/0,
		current_filename/1,
		load_data/1
	]).

:- dynamic(isHittable/2).
:- dynamic(isAnchorPointFor/2).
:- dynamic(isCollapsable/1).
:- dynamic(isBelow/2).
:- dynamic(isLeft/2).
:- dynamic(isOn/2).
:- dynamic(isOver/2).
:- dynamic(isRight/2).
:- dynamic(isTower/1).
:- dynamic(belongsTo/2).
:- dynamic(bird/1).
:- dynamic(birdOrder/2).
:- dynamic(canCollapse/2).
:- dynamic(canExplode/2).
:- dynamic(collapsesInDirection/3).
:- dynamic(ground_plane/1).
:- dynamic(hasColor/2).
:- dynamic(hasForm/2).
:- dynamic(hasOrientation/2).
:- dynamic(hasSize/2).
:- dynamic(hill/5).
:- dynamic(parabola/5).
:- dynamic(pig/5).
:- dynamic(protects/2).
:- dynamic(scene_scale/2).
:- dynamic(shape/6).
:- dynamic(situation_name/1).
:- dynamic(slingshotPivot/2).
:- dynamic(structure/1).
:- dynamic(supports/2).
:- dynamic(hasMaterial/6).
:- dynamic(current_filename/1).
:- dynamic(sceneRepresentation/1).
object(X) :- hasMaterial(X,_,_,_,_,_).
object(X) :- hill(X,_,_,_,_).
object(X) :- pig(X,_,_,_,_).

purge :-
	retractall(isHittable(_,_)),
	retractall(isAnchorPointFor(_,_)),
	retractall(isCollapsable(_)),
	retractall(isBelow(_,_)),
	retractall(isLeft(_,_)),
	retractall(isOn(_,_)),
	retractall(isOver(_,_)),
	retractall(isRight(_,_)),
	retractall(isTower(_)),
	retractall(belongsTo(_,_)),
	retractall(bird(_)),
	retractall(birdOrder(_,_)),
	retractall(canCollapse(_,_)),
	retractall(canExplode(_,_)),
	retractall(collapsesInDirection(_,_,_)),
	retractall(ground_plane(_)),
	retractall(hasColor(_,_)),
	retractall(hasForm(_,_)),
	retractall(hasOrientation(_,_)),
	retractall(hasSize(_,_)),
	retractall(hill(_,_,_,_,_)),
	retractall(parabola(_,_,_,_,_)),
	retractall(pig(_,_,_,_,_)),
	retractall(protects(_,_)),
	retractall(scene_scale(_,_)),
	retractall(shape(_,_,_,_,_,_)),
	retractall(situation_name(_)),
	retractall(sceneRepresentation(_)),
	retractall(slingshotPivot(_,_)),
	retractall(structure(_)),
	retractall(supports(_,_)),
	retractall(hasMaterial(_,_,_,_,_,_)).

load_data(Filename) :-
	((
			current_filename(CurrentFilename),
			unload_file(CurrentFilename),
			retractall(current_filename(_))
		) ->
		true;true),
	purge,
	consult(Filename),
	asserta(current_filename(Filename)).