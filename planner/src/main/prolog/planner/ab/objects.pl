:- module(ab_objects, [
		hill/1,
		pig/1,
		hasMaterial/2,
		redbird/1,
		bluebird/1,
		bluebird/1,
		blackbird/1,
		whitebird/1,
		num_of_pigs_remaining/1,
		num_of_birds_remaining/1,
		abType/2,
		destroyable/1,
		height/2,
		width/2
	]).
:- use_module(planner(data)).

hill(H) :-
	hill(H,_,_,_,_).

pig(P) :-
	pig(P,_,_,_,_).

hasMaterial(O,M) :-
	hasMaterial(O,M,_,_,_,_).

destroyable(O) :-
	abType(O, Type),
	(between(9, 12, Type); Type = 18).


% defines birds
redbird(Bird) :-
	hasColor(Bird, red).

yellowbird(Bird) :-
	hasColor(Bird, yellow).

bluebird(Bird) :-
	hasColor(Bird, blue).

blackbird(Bird) :-
	hasColor(Bird, black).

whitebird(Bird) :-
	hasColor(Bird, white).

num_of_pigs_remaining(N) :-
	findall(P,pig(P),Pigs), length(Pigs,N).

abType(O, ID) :-
	hill(O),
	ID is 2, !.
abType(O, ID) :-
	redbird(O),
	ID is 4, !.
abType(O, ID) :-
	yellowbird(O),
	ID is 5, !.
abType(O, ID) :-
	bluebird(O),
	ID is 6, !.
abType(O, ID) :-
	whitebird(O),
	ID is 8, !.
abType(O, ID) :-
	pig(O),
	ID is 9, !.
abType(O, ID) :-
	hasMaterial(O, ice, _, _, _, _),
	ID is 10, !.
abType(O, ID) :-
	hasMaterial(O, wood, _, _, _, _),
	ID is 11, !.
abType(O, ID) :-
	hasMaterial(O, stone, _, _, _, _),
	ID is 12, !.
abType(O, ID) :-
	hasMaterial(O, tnt, _, _, _, _),
	ID is 18, !.

/**
 * num_of_birds_remaining(-N) is det
 * 
 * The number of birds remaining in the scene	
 */
num_of_birds_remaining(N) :-
	findall(B,bird(B),Birds), length(Birds,N).

height(Object, H) :-
	hasMaterial(Object, _,_,_,_,H).
width(Object, W) :-
	hasMaterial(Object, _,_,_,W,_).
