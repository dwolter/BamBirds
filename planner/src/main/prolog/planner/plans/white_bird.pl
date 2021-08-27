:- module(plans_whitebird, [white_bird/5, plans_common:plan/2]).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).
:- use_module(planner(physics)).
:- use_module(planner(ab/relations)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% WhiteBird Targeting (based on unused 2020 version) %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:42, strategy:"whitebirdtargeting", confidence:Conf, reasons:Pigs}) :-
	hasColor(Bird,white),
	in_slingshot(Bird),
	white_bird(Bird, Target, Shot, Conf, Pigs).
	% Conf is 4.0,
	% Target = pig0,
	% Pigs = [pig0],
	% UnconvertedShot = [486, 276, 0.5539913058196878, -0.0012450537106506983, 0.6186103437782923, [], 1713.041753492272, -825, 565],
	% convert_to_shot(UnconvertedShot, Shot).

white_bird(Bird, Target, ShotDict, Confidence, Pigs) :-
	gather_polygon_coordinates(PolygonList),
	establish_attack_space(PolygonList, AttackPoints),
	calculate_shots(Bird, AttackPoints, AttackPointsWithShots), !,
	% simulate_explosion_for_shots( AttackPointsWithShots, AttackPointsWithConfidence ), !,
	% maplist(calculate_confidence, AttackPointsWithShots, AttackPointsWithConfidence), !,
	% member((Target, _, Shot, Pigs, Confidence), AttackPointsWithConfidence),
	% confident_enough(Confidence),
	% best_shot(AttackPointsWithConfidence, BestShot),
	% BestShot = (Target, _, Shot, Pigs, Confidence),
	member((Target, [X,Y], Shot), AttackPointsWithShots),
	calculate_confidence(Target, [X, Y], Shot, Confidence, Pigs),
	convert_to_shot(Shot, ShotDict).

best_shot([S], S).
best_shot([First|Shots], Best) :-
	First = (_, _, _, _, Confidence),
	best_shot(Shots, BestRest),
	BestRest = (_, _, _, _, ConfidenceRest),
	(Confidence > ConfidenceRest ->
		Best = First;
		Best = BestRest
	).

% Debug function to write the targets computed for a given situation
% to the file 'doc/Project\ Reports/2021/white-birds/visualization/input.txt'.
print_potential_targets() :-
	tell('doc/Project Reports/2021/white-birds/visualization/input.txt'),
	set_prolog_flag(answer_write_options,[max_depth(0)]),
	gather_polygon_coordinates(PolygonList),
	establish_attack_space(PolygonList, AttackPoints),
	calculate_shots(_, AttackPoints, AttackPointsWithShots),
	findall((Target, [TX, TY], Shot, Pigs, Confidence), (
		member((Target, [TX,TY], Shot), AttackPointsWithShots),
		calculate_confidence(Target, [TX, TY], Shot, Confidence, Pigs)
	), ConfidentAttacks),
	write(PolygonList),
	nl,
	write(AttackPoints),
	nl,
	write(AttackPointsWithShots),
	nl,
	write(ConfidentAttacks),
	told().

% Whether or not a confidence is above a general confidence threshold.
% TODO: Find suitable threshold.
confident_enough(Confidence) :-
	Confidence > 0.3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gather polygon coordinates %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieve a list of all polygons in the level that are not birds.
% Shapes such as balls are simplified to low-poly versions.
% All polygons coordinates are orderded counter-clockwise.
% The coordinates are labeled by their respective shapes.
%
% @returns (shape_name, [polygon_coordinates])
gather_polygon_coordinates(PolygonList) :-
	generate_collision_shapes(),
	findall( shape(Name, Form, CX, CY, Mass, Coordinates), not_a_bird(Name, Form, CX, CY, Mass, Coordinates), ShapeList),
	maplist(extract_shape_coordinates, ShapeList, PolygonList).

not_a_bird(Name, Form, CX, CY, Mass, Coordinates) :-
	shape(Name, Form, CX, CY, Mass, Coordinates),
	not(bird(Name)).

% Convert ball shapes to octagon coordinates.
% Mainly developed by Dominik Weiß (2020).
% @returns (shape_name, [polygon_coordinates])
extract_shape_coordinates(shape(Name, ball, CX, CY, _, [Radius]), Polygon) :- 
	% diagonal distance for x and y 
	Pythdist is sqrt((Radius^2)/2),                                    
	Farleft is CX-Radius,
	Lightleft is CX-Pythdist,
	Lightright is CX+Pythdist,
	Farright is CX+Radius,
	Farup is CY+Radius,
	Lightup is CY+Pythdist,
	Lightdown is CY-Pythdist,
	Fardown is CY-Radius,
	% beginning with the two last coordinates
	append( [(CX,        Fardown  )], [(Lightleft, Lightdown)], EC1),
	append( [(Lightright,Lightdown)], EC1, EC2),
	append( [(Farright,  CY       )], EC2, EC3),
	append( [(Lightright,Lightup  )], EC3, EC4),
	append( [(CX,        Farup    )], EC4, EC5),
	append( [(Lightleft, Lightup  )], EC5, EC6),
	append( [(Farleft,   CY       )], EC6, ExtractedCoord),
	Polygon = (Name, ExtractedCoord).

% Convert rectangular shapes to rectangle coordinates.
% Mainly developed by Dominik Weiß (2020).
% @returns (shape_name, [polygon_coordinates])
extract_shape_coordinates(shape( Name, rect, CX, CY, _, [Height, Width, Angle]), Polygon) :-
	Left is -(Width/2),
	Right is Width/2,
	Up is -(Height/2),
	Down is Height/2,
	X1 is Left,
	Y1 is Down,
	X2 is Right,
	Y2 is Down,
	X3 is Right,
	Y3 is Up,
	X4 is Left,
	Y4 is Up,
	% x' = cos(deg) * x + sin(deg) * y
	% y' = -sin(deg) * x + cos(deg) * y
	X1N is cos(-Angle) * X1 + sin(-Angle) * Y1 + CX,
	X2N is cos(-Angle) * X2 + sin(-Angle) * Y2 + CX,
	X3N is cos(-Angle) * X3 + sin(-Angle) * Y3 + CX,
	X4N is cos(-Angle) * X4 + sin(-Angle) * Y4 + CX,
	Y1N is -sin(-Angle) * X1 + cos(-Angle) * Y1 + CY,
	Y2N is -sin(-Angle) * X2 + cos(-Angle) * Y2 + CY,
	Y3N is -sin(-Angle) * X3 + cos(-Angle) * Y3 + CY,
	Y4N is -sin(-Angle) * X4 + cos(-Angle) * Y4 + CY,
	Polygon = (Name, [(X1N,Y1N), (X2N,Y2N), (X3N, Y3N), (X4N, Y4N)]).

% Convert TNT packets to a rectangle.
% @returns (shape_name, [polygon_coordinates])
extract_shape_coordinates(shape(Name, unknown, CX, CY, _, []), Polygon) :-
	canExplode(Name, _),
	Width is 15,
	Height is 15,
	Area is Width * Height,
	extract_shape_coordinates(shape(Name, rect, CX, CY, Area, [Width, Height, 0]), Polygon).

% Convert polygons to lists of coordinates.
% Mainly developed by Dominik Weiß (2020).
% @returns (shape_name, [polygon_coordinates])
extract_shape_coordinates(shape(Name, _, _, _, _, [_|Coord]), Polygon) :-
	% Except for the first element, coordinates are taken as they are.
	maplist(make_tuple, Coord, ExtractedCoord),
	Polygon = (Name, ExtractedCoord).

make_tuple([X,Y], (X,Y)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Establish Attack Space %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieve a list of potential attack targets. Targets are defined as points
% on a top surface of a polygon or on the ground. There is an upper bound for 
% the distance between two neighboring points on the same surface line, defined 
% in split_attack_space_rec.
% If the ground is covered by a polygon, then this part is excluded from the
% attack space. Objects attack surfaces are not filtered in any way.
%
% @returns [(shape_name, [target_x, target_y])]
establish_attack_space(PolygonList, AttackPoints) :-
	ground_attack_space(PolygonList, AttackSpaceGround),
	object_attack_space(PolygonList, AttackSpaceObjects),
	append(AttackSpaceGround, AttackSpaceObjects, AttackSpace),
	split_attack_space(AttackSpace, AttackPoints).

% Find open ground between structures.
% Mainly developed by Dominik Weiß.
% @returns [(ground, [x1, y1, x2, y2])]
ground_attack_space(PolygonList, OpenGroundWithName) :-
	ground_plane(GroundLevel),
	filter_ground_ranges(PolygonList, GroundLevel, AllRanges),
	sort_ranges_wb(AllRanges, [FirstRange | SortedRanges]),
	calculate_covered_ground(SortedRanges, FirstRange, CoveredGround),
	calculate_open_ground(CoveredGround, 0, GroundLevel, OpenGround),
	maplist(add_ground_name, OpenGround, OpenGroundWithName).

add_ground_name((X1,Y1,X2,Y2), (ground, [X1,Y1,X2,Y2])).

% Find ground ranges covered by objects.
% Mainly developed by Dominik Weiß.
filter_ground_ranges([], _, []).
filter_ground_ranges([(_, Polygon)|PolygonList], GroundLevel, AllRanges) :-
	is_object_on_ground(Polygon, GroundLevel, TrueOrNot),
	filter_ground_ranges(PolygonList, GroundLevel, RestRanges),
	( TrueOrNot ->
		left_right_edges(Polygon, GroundLevel, PolygonRange),
		append( [PolygonRange], RestRanges, AllRanges);
		AllRanges = RestRanges
	).

% Check whether an object is on the ground. This is the case if its highest
% Y coordinate (measured from top of the screen) is at most 20 px above the ground.
% Mainly developed by Dominik Weiß.
is_object_on_ground( [], _, TrueOrNot) :- TrueOrNot = fail .
is_object_on_ground( [(_,Y)|RestPolygonPoints], GroundLevel, TrueOrNot) :-
    AdjustedGroundLevel is GroundLevel - 20,
    ( AdjustedGroundLevel =< Y ->
	    TrueOrNot = true;
			is_object_on_ground(RestPolygonPoints, GroundLevel, TrueOrNot)
		).

% Compute the bottom-most-left and right-most points in a polygon.
% Mainly developed by Dominik Weiß.
left_right_edges( [], _, (LX,RX)) :- LX = 2000 , RX = -1000 .
left_right_edges( [(PX,PY)|RestPolygonPoints], GroundLevel, (LX, RX) ) :-
	left_right_edges( RestPolygonPoints, GroundLevel, (CurrLX, CurrRX) ),
	( PY =< GroundLevel, PX < CurrLX -> LX is PX; LX is CurrLX ),
	( PX >= CurrRX                   -> RX is PX; RX is CurrRX ).

% Sort ground ranges by their leftmost point.
% Mainly developed by Dominik Weiß.
sort_ranges_wb([],[]).
sort_ranges_wb([(L,R)|RestRanges], SortedRanges) :-
	rangepivot((L,R),RestRanges,P1,P2), sort_ranges_wb(P1, SortedR1), sort_ranges_wb(P2, SortedR2),
	append(SortedR1,[(L,R)|SortedR2], SortedRanges).

rangepivot((_,_),[],[],[]).
rangepivot((L1,R1),[(L2,R2)|RestRanges],[(L2,R2)|P1],P2):- L2=<L1,rangepivot((L1,R1),RestRanges,P1,P2).
rangepivot((L1,R1),[(L2,R2)|RestRanges],P1,[(L2,R2)|P2]):- L2>L1,rangepivot((L1,R1),RestRanges,P1,P2).

% Calculate the part of the ground that is covered by structures.
% Mainly developed by Dominik Weiß.
calculate_covered_ground([], CurrentRange, [CurrentRange]).
calculate_covered_ground([ (LX2,RX2) |AllRanges], (LX1,RX1), CoveredGround) :-
	( RX1 >= LX2 ->        
		% The new range overlaps with already treated faces.
		( RX1 < RX2 ->     
			% The new range extends further to the right.
			% Combine both ranges.
			calculate_covered_ground( AllRanges, (LX1,RX2), CoveredGround);
			% Otherwise, use old range because the new one is contained in it.
			calculate_covered_ground( AllRanges, (LX1,RX1), CoveredGround) 
		);
		% Collect different ranges.
		calculate_covered_ground( AllRanges, (LX2,RX2), RestCoveredGround),
		% Archive old ranges.
		append([(LX1,RX1)], RestCoveredGround, CoveredGround)
	).

% Calculate the part of the ground that is not covered by structures.
% Mainly developed by Dominik Weiß.
calculate_open_ground([], StartPoint, GroundLevel, OpenGround) :-
	( 1000 > StartPoint ->
	  OpenGround = [(StartPoint,GroundLevel,1000,GroundLevel)];
		OpenGround = []
	).
calculate_open_ground( [(LeftPoint, RightPoint)|CoveredGround], StartPoint, GroundLevel, OpenGround) :- 
	% only X coordinates
	calculate_open_ground( CoveredGround, RightPoint, GroundLevel, RestOpenGround),
	append( [(StartPoint,GroundLevel,LeftPoint,GroundLevel)], RestOpenGround, OpenGround).


% Compute possible attack points for the given polygons.
% Mainly developed by Dominik Weiß.
object_attack_space([],[]).
object_attack_space([(Name, [(PX,PY)|Polygon]) |PolygonList], AttackSpace) :-
	object_attack_space(PolygonList, RestAttackSpace),
	rightleaning_vectors(Name, (PX,PY), Polygon, ((LX,LY), LocalAttackSpace) ),
	( LX < PX ->                                                         
		% Vektor von Endpunkt zu Startpunkt muss extra getestet werden
		append([(Name,[LX,LY,PX,PY])|LocalAttackSpace], RestAttackSpace, AttackSpace);
		append(LocalAttackSpace, RestAttackSpace, AttackSpace )
	).

% Get the right-leaning vectors inside a polygon.
% Mainly developed by Dominik Weiß.
rightleaning_vectors(Name, (X1,Y1), [(X2,Y2)|PolygonCoords], (EndPoint, Vectors)):-
	% Account for the (implicit) connection between the last and the first vertex
	% of a polygon.
	append(PolygonCoords, [(X1,Y1)], ExtendedPolygonCoords),
	rightleaning_vectors_rec( Name, (X1,Y1), [(X2,Y2)|ExtendedPolygonCoords], (EndPoint, Vectors)).

rightleaning_vectors_rec( _, (X,Y), [], ((X,Y),[]) ).
rightleaning_vectors_rec( Name, (X1,Y1), [(X2,Y2)|PolygonCoords], (EndPoint, Vectors) ) :-
	% A threshold to reject vectors if they are mostly vertical, i.e., their X
	% values change by at most MinDeltaX.
	MinDeltaX = 3,
	rightleaning_vectors_rec(Name, (X2,Y2), PolygonCoords, (EndPoint, RestVectors)),
	( (X1 - X2) > MinDeltaX ->
		append( [(Name, [X1,Y1,X2,Y2])], RestVectors, Vectors );
		Vectors = RestVectors
	).

% Select points on the given attack space such that the distance between
% them is at most the threshold specified in split_attack_space_rec.
% @returns [(shape_name, [target_x, target_y])]
split_attack_space([],[]).
split_attack_space(AttackSpace, AttackPoints) :-
	split_attack_space_rec(AttackSpace, [], AttackPoints).

split_attack_space_rec([], Result, Result).
split_attack_space_rec([(Name, [X1,Y1,X2,Y2])| RestVectors], Points, Result) :-
	% TODO: Find a suitable threshold.
	MaxDeltaX is 5,
	(MaxDeltaX < (X1 - X2) ->
		XNew is round(X1 - MaxDeltaX),
		YNew is round(Y1 + (Y2 - Y1) * (MaxDeltaX / (X1 - X2))),
		split_attack_space_rec([(Name, [XNew, YNew, X2, Y2])|RestVectors], [(Name, [X1,Y1])|Points], Result);
		split_attack_space_rec(RestVectors, [(Name, [X1, Y1]), (Name, [X2, Y2]) | Points], Result)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculate Available Shots %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Retrieve a list of potential shots to dropping points above attack points.
% Check for reachability by incrementally searching for …
% 	- obstacles between dropping and attack point
%	  - obstacles on the trajectory to the dropping point
calculate_shots(_, [], []).
calculate_shots(BirdName, AttackPoints, AttackPointsWithShotsReversed) :-
	findall(
		AttackPointWithShot,
		is_reachable(BirdName, AttackPoints, AttackPointWithShot),
		AttackPointsWithShots
	),
	reverse(AttackPointsWithShots, AttackPointsWithShotsReversed).

% Find non-blocked shots with the given bird at one of the AttackPoints.
is_reachable(BirdName, AttackPoints, AttackPointWithShot) :-
	member((Target,[X,Y]), AttackPoints),
	lowest_possible_shot(BirdName, (Target,[X,Y]), Y, AttackPointWithShot).

% Step size for the computation of the lowest possible shot.
% TODO: Find suitable value.
upwards_iteration_step_size(20).

% Find the lowest possible shot at a location above the point (X, Y),
% such that the egg can freely drop onto (X, Y).
lowest_possible_shot(BirdName, (Target, [X,Y]), Height, AttackPointWithShot) :-
	upwards_iteration_step_size(StepSize),
	Height > StepSize,
	NewHeight is Height - StepSize,
	upwards_step_free(X, Height, NewHeight, Target),
	shots_at_point(BirdName, X, NewHeight, AvailableShots),
	obstacle_free_shots(AvailableShots, ObstacleFreeShots),	
	(
		(ObstacleFreeShots = [Shot|_], 
		AttackPointWithShot = (Target, [X,Y], Shot)), !
		;
		lowest_possible_shot(BirdName, (Target, [X,Y]), NewHeight, AttackPointWithShot)
	).

egg_radius(8).

% Whether or not there is an obstacle on the vertical line between LowerY and 
% UpperY, located at the given X coordinate.
% The target object is excluded from the list of obstacles to enable target 
% points on skewed edges
upwards_step_free(X, LowerY, UpperY, Target) :-
	egg_radius(EggRadius),
	BottomThreshold is 2,
	LowerYWithThreshold is LowerY - BottomThreshold,
	LeftX is X - EggRadius,
	RightX is X + EggRadius,
	\+ drop_point_obstacle([LeftX, X, RightX], LowerYWithThreshold, UpperY, Target),
	\+ (
		shape(_, ShapeType, CX, CY, _, ShapeData),
		line_crosses_shape(ShapeType, CX, CY, ShapeData, LeftX, UpperY, RightX, UpperY, _, _)
	).

% Find all obstacles on the vertical lines between (X, LowerY) and (X, UpperY)
% for any X in Xs.
drop_point_obstacle(Xs, LowerY, UpperY, Target) :-
	member(X, Xs),
	object_on_vline(Obstacle, X, UpperY, LowerY, _),
	Obstacle \= Target.

obstacle_free_shots([], []).
obstacle_free_shots([[TX,TY, Angle, A, B, Obstacles, TimeOfFlight, RX, RY] | Shots], FreeShots) :-
	obstacle_free_shots(Shots, RecursiveFreeShots),
	([]==Obstacles ->
		append([[TX,TY, Angle, A, B, Obstacles, TimeOfFlight, RX, RY]], RecursiveFreeShots, FreeShots);
		FreeShots = RecursiveFreeShots
	).


convert_to_shot([TX, TY, _, _, _, _, TimeOfFlight, ReleaseX, ReleaseY], shot{sling_x:SlingX, sling_y:SlingY, drag_x:ReleaseX, drag_y:ReleaseY, target_x:TX, target_y:TY, tap_time:TapTime}) :-
	slingshotPivot(X0,Y0),
	SlingX is round(X0),
	SlingY is round(Y0),
	TapTime is round(TimeOfFlight * 0.99).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Explosionsradius Algorithmus:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simulier entlang der Angriffsfläche den Effekt der Explosion in regelmäßigen Abstand.
%% Vektoren, die horizontal zu lang sind werden gespalten. Merk immer den besten Wert für einen Vektor.

simulate_explosion_for_shots(AttackPointsWithShots, EvaluatedShots) :-
	findall(ShotWithVictims, simulate_explosion(AttackPointsWithShots, ShotWithVictims), ShotsWithVictims),
	maplist(calculate_damage, ShotsWithVictims, EvaluatedShots).
%TODO: Sortieren
%        sortbyconf( RestZielListe, Besttarget, ZielListe )           %% Neues Ziel wird in Liste einsortiert

%% Explosion simulieren, indem erst alle Objekte im Radius gesammelt werden und dann conf-Werte fuer diese Objekte ermitteln und ausgeben
simulate_explosion(AttackPointsWithShots, ShotWithVictims) :-
	member((Target, [X,Y], Shot), AttackPointsWithShots),
	findall( shape(Name, Form, CX, CY, Mass, Coord), shape(Name, Form, CX, CY, Mass, Coord), AllObjects),
	findall(ObjectWithinRadius, within_explosion_radius( (X,Y), AllObjects, ObjectWithinRadius), ObjectsWithinRadius),
	findall(Pig, (member(Pig, ObjectsWithinRadius), pig(Pig, _, _, _, _)), Pigs),
	ShotWithVictims = (Target, [X,Y], Shot, ObjectsWithinRadius, Pigs).

%% Ob sich der Mittelpunkt eines Objektes in der Explosion befindet
within_explosion_radius( (CX,CY), AllObjects, ObjectWithinRadius) :-
    member(shape(ObjectWithinRadius,_,OX,OY,_,_), AllObjects),
	DistSquared is (CX-OX)^2 + (CY-OY)^2,
	ThresholdRadiusSquared is 45^2, %% 45 ist Schaetzwert, fuer den Radius der effektiven Explosionszone; Vergleich: kl.Pig:r=5, gr.Pig:r=10
	DistSquared < ThresholdRadiusSquared.


%% Conf-Werte aller Objekte zusammenrechnen
calculate_damage( (Target, [X,Y], Shot, ObjectsWithinRadius, Pigs),  (Target, [X,Y], Shot, Pigs, Damage)) :-
	calculate_damage_rec(ObjectsWithinRadius, Damage).

calculate_damage_rec([], 0).
calculate_damage_rec([ObjectWithinRadius|RestObjects], TotalDamage):-
	calculate_damage_rec(RestObjects, RestDamage),
	whitebird_object_conf(ObjectWithinRadius, Damage),
	TotalDamage is Damage + RestDamage.

% TODO Braucht weit mehr Verfeinerung; Ist auch ineffektiv für taktischere Züge
%% Conf-Wert fuer Objekt in WhiteBird-Explosion;
%% Pigs and TNT are valuable targets; Bars support structures and cause collapses;
%% WB can destroy Stone, so it can be a required tactic in some levels;
%% TODO: Bisherige Conf-Werte nur grobe Schätzungen, mach bessere Beurteilungen;
%% TODO: Verbessere Conf-Wert-Zuweisungen mit Daten aus den situationx-x.pl wie:
%% hasOrientation(), hasSize(), so wie die ganzen strukturbeschreibenden Angaben
% whitebird_object_conf(Object, 1.0) :- pig(Object,_,_,_,_), !.
% whitebird_object_conf(Object, 0.0) :- hill(Object,_,_,_,_), !.
% whitebird_object_conf(Object, 0.8) :- hasMaterial(Object, tnt, _, _, _, _), !.
% whitebird_object_conf(Object, 0.01) :- hasMaterial(Object, ice, _, _, _, _), hasForm(Object, bar), !.
% whitebird_object_conf(Object, 0.002) :- hasMaterial(Object, ice, _, _, _, _), !.
% whitebird_object_conf(Object, 0.03) :- hasMaterial(Object, wood, _, _, _, _), hasForm(Object, bar), !.
% whitebird_object_conf(Object, 0.01) :- hasMaterial(Object, wood, _, _, _, _), !.
% whitebird_object_conf(Object, 0.08) :- hasMaterial(Object, stone, _, _, _, _), hasForm(Object, bar), !.
% whitebird_object_conf(Object, 0.02) :- hasMaterial(Object, stone, _, _, _, _), !.
% whitebird_object_conf(_, 0.0).  %% Wenn Objekt in keine der Kategorien passt.


%% Sortiert neuen Eintrag in Liste von Angriffstellen nach conf-Wert; Liste ist bereits vorsortiert
sortbyconf( [], Newtarget, SortedList ) :-
    append( [Newtarget], [], SortedList ).
sortbyconf( [(TX,TY,TConf)|Targetlist], (NX,NY,NConf), SortedList ) :-
    ( TConf < NConf ->
        append( [(NX,NY,NConf)], [(TX,TY,TConf)|Targetlist], SortedList );
        sortbyconf( Targetlist, (NX,NY,NConf), RestSortedList),
        append( [(TX,TY,TConf)], RestSortedList, SortedList )
	).


first_of_list([F|_], F).
second_of_list([_,S|_], S).

% Takes AttackPointWithShot and returns EvaluatedShot
% calculate_confidence((Target, [X,Y], Shot), (Target, [X,Y], Shot, Pigs, Confidence)) :-
calculate_confidence(Target, [X, Y], _, Confidence, PigsSet) :-
	findall([Name, CX, CY], (pig(Name, _, _, _, _), shape(Name, _, CX, CY, _, _)), AllPigs),
	findall([Conf, Pigs], whitebird_conf(Target, X, Y, AllPigs, Conf, Pigs), Strategies),
	maplist(first_of_list, Strategies, Confs),
	maplist(second_of_list, Strategies, PigsListList),
	flatten(PigsListList, PigsList),
	list_to_set(PigsList, PigsSet),
	max_list(Confs, Confidence).

% Strategy: Direct-Hit! drop egg directly next to a pig to kill it. 
% Objects in between have no influence and can here be neglected.
whitebird_conf(_, X, Y, AllPigs, Confidence, Pigs) :-
	explosion_radius_inner(RadiusInner),
	explosion_radius_outer(RadiusOuter),
	RadiusInnerSquared is RadiusInner ^ 2,
	RadiusOuterSquared is RadiusOuter ^ 2,
	findall([Conf, Pig], (
		member([Pig, CX, CY], AllPigs),
		DistanceSquared is ((X - CX) ^ 2 + (Y - CY) ^ 2),
		(DistanceSquared < RadiusInnerSquared ->
			Conf is 1.0;
			DistanceSquared < RadiusOuterSquared,
			Conf is 1.0 - ((DistanceSquared - RadiusInnerSquared) / (RadiusOuterSquared - RadiusInnerSquared)) * 0.2
	)), Within),
	maplist(first_of_list, Within, Confs),
	maplist(second_of_list, Within, Pigs),
	length(Pigs, NumberOfPigs),
	NumberOfPigs > 0,
	sumlist(Confs, ConfSum),
	Confidence is (ConfSum / NumberOfPigs) * (1.0 + (NumberOfPigs - 1) * 0.3).

% Strategy: Damage! A pig is too far away but a lot of damage is done in its surounding.
% Something has to be in the direct explision radius though.
whitebird_conf(_, X, Y, AllPigs, Conf, [Pig]) :-
	member([Pig, _, _], AllPigs),
	explosion_radius_outer(ExplosionRadius),
	collateral_damage_radius(CollateralRadius),
	within_radius(Pig, X, Y, CollateralRadius),
	findall(Object, 
		(shape(Object, _, _, _, _, _), within_radius(Object, X, Y, ExplosionRadius)),
		ObjectsWithinExplosion),
	maplist(whitebird_object_conf, ObjectsWithinExplosion, ExplosionConfs),
	sumlist(ExplosionConfs, ExplosionSum),
	ExplosionSum > 0.1,
	findall(Object, 
		(shape(Object, _, _, _, _, _), within_radius(Object, X, Y, CollateralRadius)),
		ObjectsWithinCollateral),
	maplist(whitebird_object_conf, ObjectsWithinCollateral, AllObjectConfs),
	sumlist(AllObjectConfs, ConfSum),
	Conf is min(ConfSum + ExplosionSum, 1.1).

% Strategy: TNT! Target TNT which blows up pig
whitebird_conf(_, X, Y, AllPigs, Conf, Pigs) :-
	hasMaterial(TNT, tnt),
	explosion_radius_outer(Radius),
	within_radius(TNT, X, Y, Radius),
	findall(Pig, (
		member([Pig, _, _], AllPigs),
		canExplode(TNT, Pig)
		), PigsList),
	list_to_set(PigsList, Pigs),
	length(Pigs, NumberOfPigs),
	Conf is 0.95 + (NumberOfPigs - 1) * 0.3.

% Strategy: TNT! Target TNT which blows up belonging structure
whitebird_conf(_, X, Y, AllPigs, Conf, Pigs) :-
	hasMaterial(TNT, tnt),
	explosion_radius_outer(Radius),
	within_radius(TNT, X, Y, Radius),
	findall(Pig, (
		canExplode(TNT, Object),
		belongsTo(Object, Structure),
		isCollapsable(Structure),
		member([Pig, _, _], AllPigs),
		(
			belongsTo(Pig, Structure);
			belongsTo(Pig, Structure2),
			canCollapse(Structure, Structure2)
		)
		), PigsList),
	list_to_set(PigsList, Pigs),
	length(Pigs, NumberOfPigs),
	Conf is 0.9 + (NumberOfPigs - 1) * 0.3.


explosion_radius_inner(35).
explosion_radius_outer(45).

collateral_damage_radius(65).

whitebird_object_conf(Object, 0.35) :- pig(Object,_,_,_,_), !.
whitebird_object_conf(Object, 0.0) :- hill(Object,_,_,_,_), !.
whitebird_object_conf(Object, 0.2) :- hasMaterial(Object, tnt, _, _, _, _), !.
whitebird_object_conf(Object, 0.005) :- hasMaterial(Object, ice, _, _, _, _), hasForm(Object, bar), !.
whitebird_object_conf(Object, 0.001) :- hasMaterial(Object, ice, _, _, _, _), !.
whitebird_object_conf(Object, 0.06) :- hasMaterial(Object, wood, _, _, _, _), hasForm(Object, bar), !.
whitebird_object_conf(Object, 0.02) :- hasMaterial(Object, wood, _, _, _, _), !.
whitebird_object_conf(Object, 0.05) :- hasMaterial(Object, stone, _, _, _, _), hasForm(Object, bar), !.
whitebird_object_conf(Object, 0.01) :- hasMaterial(Object, stone, _, _, _, _), !.
whitebird_object_conf(_, 0.0).  %% Wenn Objekt in keine der Kategorien passt.

within_radius(Object, X, Y, Radius) :-
	shape(Object, _, CX, CY, _, _),
	DistanceSquared is ((X - CX) ^ 2 + (Y - CY) ^ 2),
	RadiusSquared is Radius ^ 2,
	DistanceSquared < RadiusSquared.

