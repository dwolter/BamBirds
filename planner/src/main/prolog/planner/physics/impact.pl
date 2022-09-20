:- module(physics_impact, [impact/3, dominoImpact/3, hasDominoImpact/3]).
:- use_module(planner(ab/objects)).
:- use_module(planner(data)).


%% Values for Energy-calculation are mapped from game/slingshot/cors/fowl/json/Materials.json to our scale.

%% Rough estimate for number of objects of a certain material that a certain type of bird can penetrate
% (mass * damage_multiplier * velocity_multiplier) / defense

bird_type_impact(red, wood, 217.9). % * 5444.76
bird_type_impact(red, ice, 272.4).
bird_type_impact(red, snow, 54.5).
bird_type_impact(red, stone, 36.3).
%bird_type_impact(red, tnt, 5). 217.904 272.38 54.476 36.31915
%bird_type_impact(red, pork, 10).

bird_type_impact(yellow, wood, 2*3241.1). % 4823.04 (3241.0828 2893.8242 385.8432 694.51776)
bird_type_impact(yellow, ice, 2*2893.8).
bird_type_impact(yellow, stone, 2*385.8).
bird_type_impact(yellow, snow, 2*694.5).
%bird_type_impact(yellow, tnt, 1.0).
%bird_type_impact(yellow, pork, 1.0).

bird_type_impact(blue, wood, 3*723.5). % 1808.64 (723.456 2893.824 434.0736 120.58203)
bird_type_impact(blue, ice, 3*2893.8).
bird_type_impact(blue, snow, 3*434.1).
bird_type_impact(blue, stone, 3*120.6).
%bird_type_impact(blue, tnt, 5).
%bird_type_impact(blue, pork, 10).

bird_type_impact(black, wood, 3014.4). % * 7536 (3014.4001 3768.0 678.24005 1507.2001)
bird_type_impact(black, ice, 3768.0).
bird_type_impact(black, snow, 678.2).
bird_type_impact(black, stone, 1507.2).
%bird_type_impact(black, tnt, 5).
%bird_type_impact(black, pork, 10).

bird_type_impact(white, wood, 2717.0). % * 8490.56 (2716.979 3396.2239 611.32025 566.0656)
bird_type_impact(white, ice, 3396.2).
bird_type_impact(white, snow, 611.3).
bird_type_impact(white, stone, 566.1).
%bird_type_impact(white, tnt, 5).
%bird_type_impact(white, pork, 10).

bird_type_impact(_, hill, 0.001).
bird_type_impact(_, pork, 500000).
bird_type_impact(_, _, 2.0).

impact(Bird, Object, Impact) :-
	hasColor(Bird, C),
	(hasMaterial(Object, M, _, _, _, _); (hill(Object), M = hill)),
	bird_type_impact(C, M, Impact),
	!.

% Domino Impact:

dominoImpact(Bird, _, 9.5) :-
	redbird(Bird).
dominoImpact(Bird, ice, 3) :-
	ab_objects:yellowbird(Bird).
dominoImpact(Bird, wood, 2) :-
	ab_objects:yellowbird(Bird).
dominoImpact(Bird, stone, 1) :-
	ab_objects:yellowbird(Bird).
dominoImpact(Bird, ice, 2) :-
	bluebird(Bird).
dominoImpact(Bird, wood, 8) :-
	bluebird(Bird).
dominoImpact(Bird, stone, 8) :-
	bluebird(Bird).

hasDominoImpact(Bird, Object, P) :-
	hasMaterial(Object, M, _, _, W, H),
	(H > W, W<26 -> dominoImpact(Bird, M, P) ; P is 1),
	!.