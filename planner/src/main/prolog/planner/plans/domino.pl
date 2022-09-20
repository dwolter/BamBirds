:- module(plans_domino, [domino/5, plans_common:plan/2]).
:- use_module(planner(ab)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(planner(physics/impact)).
:- use_module(common).
:- use_module(planner(data)).

	
plans_common:plan(Bird,plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"domino", confidence:C, reasons:Reasons}) :-
	\+ whitebird(Bird),
	\+ blackbird(Bird),
	domino(Bird, Target, UUID, C, Reasons),
	>(C, 0.49),
	shot_params_dict(UUID, Shot, ImpactAngle).

%domino plan, increse confidence with more structures that collapse on another

increseStructuresAffected(StructuresAffected, NewStructuresAffected) :-
	NewStructuresAffected is StructuresAffected + 1.

% collapsingOnOther_(+Structure1, +Structure2, +StructuresAffected, +Energy, -Confidence)
collapsingOnOther(Bird, Target, Structure1, Structure2, StructuresAffected, Energy, C) :-
	%isTower(Structure1),
	collapsesInDirection(Structure1, Structure2, away),
	isTower(Structure2),
	hasDominoImpact(Bird, Target, P),
	increseStructuresAffected(StructuresAffected, NewStructuresAffected),
	collapsingOnOther(Bird, Target, Structure2, _Structure3, NewStructuresAffected, Energy + ((100 - Energy) - (Energy / P)), C),
	!.

collapsingOnOther(_Bird, _Target, _Structure1, _Structure2, StructuresAffected, _Energy, C) :-
	StructuresAffected == 2,
	C is 0.5,
	!.

collapsingOnOther(_Bird, _Target, _Structure1, _Structure2, _StructuresAffected, Energy, C) :-
	C is Energy / 100,
	!.

% rule to handle single domino: drop object on target
domino(_, Target, UUID, 0.8, Reasons) :-
	is_goal(Object, _),
	\+isHittable(Object, _),
	hasMaterial(Target, _, X, Y, W, H),
	hasMaterial(Object, _, OX, OY, _, _),
	OY > Y,
	XH is X+H,
	between(X, XH, OX),
	isHittable(Target, UUID),
	parabola(Target, UUID,_,Angle,_,_),
	RoundedAngle is round(Angle),
	between(-50,50, RoundedAngle),
	>(H, 1.5*W),
	XLeft is X+W+4,
	XRight is X+W+W+8,
	YDown is max(OY-2, Y+H),
	forall(in_vertical_corridor(O, XLeft, XRight, Y, YDown), is_goal(O,_)),
	(pig(Object) -> Pigs = [Object] ; Pigs = []),
	merge_reasons(Pigs, [], [], Reasons).


% domino_(+Bird, -Target, -Confidence)
domino(Bird, Target, UUID, C, Reasons) :-
	object(Target),
	\+pig(Target),
	in_slingshot(Bird),
	flachschuss(Target, UUID),
	parabola(Target, UUID,_,Angle,_,_),
	RoundedAngle is round(Angle),
	between(-35,35,RoundedAngle),
	belongsTo(Target, Structure1),
	% This seems wrong?
	% findall(A,isHittable(Target,A), AS), % flat shot if possible -> Hat den meisten Effekt hinsichtlich des umkippens
	% max_list(AS,Angle),
	(((isCollapsable(Structure2); pig(Structure2)),
	not(Structure1==Structure2),
	((worth(Structure1) ; worth(Structure2))),
	%collapsingOnOther(Bird, Target, Structure1, Structure2, 1, 60, C),
	structure_bounding_box(Structure1, _, S1Y0, S1X1, S1Y1),
	structure_bounding_box(Structure2, S2X0, _, _, S2Y1),
	S1X1 < S2X0,             % s1 starts left of s2
	S1X1+(S1Y1-S1Y0) > S2X0, % s1 can fall on s2
	S2Y1 > S1Y0,             % s2 not above s1
	isTower(Structure1));
	structHasTwoTowers(Structure1, MiddleOfStructVert)),
	hasOrientation(Target,horizontal),
	hasMaterial(Target,M,X,_,L,_),
	(structHasTwoTowers(Structure1, MiddleOfStructVert) -> (X+L)<MiddleOfStructVert; true), % Hilfsfunktion: Beschreiung s. unten
	dominoImpact2(Bird, M, I), % Domino Impact orientiert am Paper von Renz et al.
	objectToTheRight(Target, R), % möglichst wenig Steine auf der rechten Seite -> kann leichter umgestoßen werden
	beatablePigsByTower(Structure1, PigsHit),% Höhere Konfidenz, umso mehr Schweine getötet werden können
	pigs_in_struct(Structure1, _, PigsInStruct),
	union(PigsHit, PigsInStruct, Pigs),
	formBonus(Target,FB), % Höhere Konfidenz, wenn Target ein Balken ist, Beschreibung s. unten
	objectsAbove(Target, N), aboveMinus(N, OA), % möglichst wenig aufliegende Steine, jedoch nicht den obersten Stein
	C is min(1, max(0, 0.12041  - R*0.03749 + FB*0.53054 - OA*0.07393   + I*0.10136)),	%Werte errechnet über lineare Regression (ml in R)
	merge_reasons([], Pigs, [], Reasons).



%dominoYesOrNo(Target, Result) :-
%	domino(_, Target, _, _, []), ! ->  Result is 1;
%	result is 0.


%%%%% Zusatzfunktion DOMINO: objektsTotheRight -> je weniger Objekte sich rechts vom Target befinden; deutet darauf hin, dass der Turm a) sehr dünn ist oder b) das Target eine wichtige Stützfunktion einnimmt (Hier gibt es also Abzug für einen Hohen Wert)
% Basesase
objectToTheRight(Target, RightBonus) :-
	\+isLeft(Target, _) -> RightBonus is 0.

% RecursionCase
objectToTheRight(Target, RightBonus) :-
	((hasMaterial(Objekt,wood,_,_,_,_); hasMaterial(Objekt,ice,_,_,_,_); hasMaterial(Objekt,stone,_,_,_,_)),
	isLeft(Target, Objekt)),
	objectToTheRight(Objekt, H)
	-> RightBonus is(1+H).

%%%%%Zusatzfunktion DOMINO: Bonus für einsturzgefährdete Schweine
beatablePigsByTower(Structure, Pigs) :-
	structHasTwoTowers(Structure, _)
	-> findall(P, (pig(P,_,_,_,_), belongsTo(P, Structure)), Pigs);
	structure(Structure),
	structure_bounding_box(Structure, _, S1Y0, S1X1, S1Y1),
	Endoffallentoweer is (S1X1+(S1Y1-S1Y0)),
	findall(P,(pig(P,S2X0,_,_,_), between(S1X1, Endoffallentoweer, S2X0)),Pigs).

%%%% Zusatzfunktion DOMINO: Bonus für bars.
%%%% Umso breiter ein Objekt relativ zu der Breite der Struktur, desto mehr Impact. Heutistische Herangehensweise über die Form.
%%%% Da nur horizontale Objeckte für die Domino-Funktion in Betracht gezogen werden, sind Balken meist gegenüber anderen Formen zu bevorzugen (nehmen die größte Breit ein)
formBonus(Object,BB):-
	hasForm(Object,bar)
	-> BB is 1;
	hasForm(Object,block)
	-> BB is 0.5;
	hasForm(Object,cube)
	-> BB is 0.

%%%% Zusatzfunktion DOMINO: Steine, die sich über dem Target befinden, satbilisieren die Struktur.
%%%% Der höchste Stein hat jedoch weniger Impact, als ein Stein, der sich fast ganz oben befindet (Erfahrungswert)
%%%% Es wird erst berechnet, wie viele Steine sich auf dem Target befinden, danach wird dieser Wert noch einmal evaluiert
objectsAbove(Target, OA):-
	\+ isOn(_,Target)
	-> OA is 0.

objectsAbove(Target,OA) :-
	(object(Object),
	isOn(Object,Target)),
	objectsAbove(Object, ObjectsAbove)
	-> OA is (1 + ObjectsAbove).

aboveMinus(0, AM):- AM = 2.
aboveMinus(N, AM):- N > 0, N =< 2, AM = 0.
aboveMinus(N, AM):- N > 2, AM = N.


%%%% Zusatzfunktion DOMINO: Die Bildverarbeitung erkennt eine Struktur, welche am Ground verbunden ist, als eine Struktur.
%%%% Hier wird getestet, ob diese eine Struktur zwei Türme hat und somit potentieller Kandidat für die DOMINO-Strategie ist
%%%% Eher ein Workaround, könnte durch Verbesserung der Szenenerkennung redundant gemacht werden
structHasTwoTowers(Structure, MiddleOfStructVert) :- 	%  MiddleOfStructHigh, X, XX, Y, YY, Object, HighOfObject) :-
	structure(Structure),
	structure_bounding_box(Structure, X0, Y0, X1, Y1),
	MiddleOfStructVert is (X0+((X1-X0)div 2)),
	%X is X0,
	%XX is X1,
	%Y is Y0,
	%YY is Y1,
	belongsTo(Object, Structure),
	\+isOn(_, Object),
	(pig(Object)->
	hasMaterial(Object,_,OX,OY,L,_),
	Lange2 is (OX + L div 2),
	findall(AnderesHoheresObject,(hasMaterial(AnderesHoheresObject,_,X2,Y2,L2,_), belongsTo(AnderesHoheresObject,Structure), Lange is (X2+L2), between(X2,Lange,Lange2), OY > Y2),HoheSachen),
	length(HoheSachen,PB),
	(PB > 0 -> false;
	isOn(Object, Darunter),
	hasMaterial(Darunter,_,O2X,O2Y,L2,H2),
	LengthOfObject is (O2X+L2),
	between(O2X, LengthOfObject, MiddleOfStructVert),
	MiddleOfStructHigh is (Y0+((Y1-Y0)div 2)),
	HighOfObject is (O2Y+H2),
	HighOfObject>MiddleOfStructHigh);
	%ELSE
	hasMaterial(Object,_,OX,OY,L,H),
	LengthOfObject is (OX+L),
	between(OX, LengthOfObject, MiddleOfStructVert),
	MiddleOfStructHigh is (Y0+((Y1-Y0)div 2)),
	HighOfObject is (OY+H),
	HighOfObject>MiddleOfStructHigh).

%%%% Domino Impact, welcher sich an den heuristischen Werten von Renz et al. orientiert
%%%% Noch zu verbessern: für den Domino-Effeckt macht es keinen Sinn wenn die Objekt beim Anschuss kaputt gehen (momentan noch 1.0)
%%%% -> TO DO: Heuristik finden, mit denen man die Werte für Domino am besten beschreiben kann
dominoImpact2(Bird, wood, I) :-
	hasColor(Bird,red)
		-> I is 0.65;
	hasColor(Bird,yellow)
		-> I is 1;
	hasColor(Bird,black)
	-> I is 1;	
	hasColor(Bird,blue)
	-> I is 0.5;	
	hasColor(Bird,white)
	-> I is 0.8.	

dominoImpact2(Bird, ice, I) :-
	hasColor(Bird,red)
	-> I is 0.8;	
	hasColor(Bird,yellow)
	-> I is 0.6;	
	hasColor(Bird,black)
	-> I is 1;	
	hasColor(Bird,blue)
	-> I is 1;	
	hasColor(Bird,white)
	-> I is 0.7.	

dominoImpact2(Bird, stone, I) :-
	hasColor(Bird,red)
	-> I is 0.3;	
	hasColor(Bird,yellow)
	-> I is 0.2;	
	hasColor(Bird,black)
	-> I is 1;	
	hasColor(Bird,blue)
	-> I is 0.25;	
	hasColor(Bird,white)
	-> I is 0.6.	

%%%%% Zusatzfunktion Ende Neu
