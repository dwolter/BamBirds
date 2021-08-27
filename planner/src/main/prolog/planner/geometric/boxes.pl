:- module(geometric_boxes, [
	bounding_box_lst/5,
	point_contained/6,
	structure_bounding_box/5,
	boxes_overlap/8,
	box_compare/3
	]).
:- use_module(planner(data)).



% compute bounding box for lists of objects
% boundingBox(+List_of_Objects, -Xmin, -Ymin, -Xmax, -Ymax)
bounding_box_lst([Object],X0,Y0,X1,Y1) :-
	hasMaterial(Object,_,X0,Y0,W,H),
	X1 is X0 + W,
	Y1 is Y0 - H.

bounding_box_lst([Object|OS],X0,Y0,X1,Y1) :-
	bounding_box_lst(OS,X0_Temp, Y0_Temp, X1_Temp, Y1_Temp),
	hasMaterial(Object, _, X0_O, Y0_O, W, H),
	X0 is min(X0_O,X0_Temp),
	Y0 is min(Y0_O,Y0_Temp),
	X1 is max(X1_Temp, X0_O+W),
	Y1 is max(Y1_Temp, Y0_O+H).

% checks containment of point in bounding box
point_contained(X,Y,X0,Y0,X1,Y1) :-
	between(X0,X1,X),
	between(Y0,Y1,Y).

% check containment of pig in structure
% structure_protects_big(+Struct, +Pig)
structure_bounding_box(Struct, X0, Y0, X1, Y1) :-
	findall(O, belongsTo(O,Struct), Objects),
	bounding_box_lst(Objects,X0,Y0,X1,Y1).


boxes_overlap(XA0, YA0, XA1, YA1, XB0, YB0, XB1, YB1) :-
	(between(XA0, XA1, XB0) ; between(XA0, XA1, XB1) ; between(XB0, XB1, XA0) ; between(XB0, XB1, XA0)), !,
	(between(YA0, YA1, YB0) ; between(YA0, YA1, YB1) ; between(YB0, YB1, YA0) ; between(YB0, YB1, YA0)), !.

box_compare(C, Obj1, Obj2) :-
	hasMaterial(Obj1,_,X1,_,_,_),
	hasMaterial(Obj2,_,X2,_,_,_),
	compare(C,X1,X2).

