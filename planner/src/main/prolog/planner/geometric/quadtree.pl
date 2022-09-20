%
% This is the quadtree implementation by Anastasia Sinitsyna and Antonia Höfer
% It is a work in progress and is not yet included in the calculations
%

:- module(geometric_quadtree, [
	object_contained/8]).
:- use_module(planner(data)).
:- use_module(boxes).

% create data structure: quadtrees
init(0,0,1000,1000). %input data for biggest quadrant

% count elements of a list
count_elements([],0).
count_elements([_],1).
count_elements([_|R],Count) :- count_elements(R,Count1), Count is Count1 + 1.

% checks if an object intersects with the quadrant
objectInQuadrant(b(X0,Y0,X1,Y1),[Object]) :-
    bounding_box_lst([Object],OX0,OY0,OX1,OY1),
    boxes_overlap(OX0,OY0,OX1,OY1,X0,Y0,X1,Y1).

% checks containment of quadrant in object Koordinaten von Quadrant, Koordinaten von Objekt
object_contained(QX0,QY0,QX1,QY1,X0,Y0,X1,Y1) :-
    point_contained(QX0,QY0,X0,Y0,X1,Y1), point_contained(QX1,QY1,X0,Y0,X1,Y1);
    point_contained(X0,Y0,QX0,QY0,QX1,QY1), point_contained(X1,Y1,QX0,QY0,QX1,QY1);
    between(X0,X1,X), between(Y0,Y1,Y), point_contained(X,Y,QX0,QY0,QX1,QY1).

% leaf of quadtree without subtrees: either empty or with one object
tree(node(b(X0,Y0,X1,Y1),[],Object)) :-
    X0 < X1,
    Y0 < Y1,
    count_elements(Object,Count),
    (Count =:= 0;
    Count =:= 1,
    bounding_box_lst([Object],OX0,OY0,OX1,OY1),
    boxes_overlap(OX0,OY0,OX1,OY1,X0,Y0,X1,Y1)).

% tree with 4 subtrees and more than one object
tree(node(b(X0,Y0,X1,Y1),[NW,NE,SW,SE],Object)) :-
    X0 < X1,
    Y0 < Y1,
    XN is (X0+X1)/2,
    YN is (Y0+Y1)/2,
    %findall(O, objectInQuadrant(b(X0,Y0,X1,Y1),[O]), Object),
    count_elements(Object,Count),
    Count > 1,
    findall(NWO, objectInQuadrant(b(X0,Y0,XN,YN),[NWO]), NWObjects),
    findall(NEO, objectInQuadrant(b(XN,Y0,X1,YN),[NEO]), NEObjects),
    findall(SWO, objectInQuadrant(b(X0,YN,XN,Y1),[SWO]), SWObjects),
    findall(SEO, objectInQuadrant(b(XN,YN,X1,Y1),[SEO]), SEObjects),
    NW = node(b(X0,Y0,XN,YN),_,NWObjects),
    tree(NW),
    NE = node(b(XN,Y0,X1,YN),_,NEObjects),
    tree(NE),
    SW = node(b(X0,YN,XN,Y1),_,SWObjects),
    tree(SW),
    SE = node(b(XN,YN,X1,Y1),_,SEObjects),
    tree(SE).

%checks if boundary is quadrant
quadrant(X0,Y0,X1,Y1) :-
    tree(node(b(X0,Y0,X1,Y1),_,_)).

%initiate tree with maximal boundary, subtrees and all objects
init_tree(node(b(X0,Y0,X1,Y1),[NW,NE,SW,SE],Object)) :-
    init(BX0,BY0,BX1,BY1),
    X0 is BX0,
    Y0 is BY0,
    X1 is BX1,
    Y1 is BY1,
    X0 < X1,
    Y0 < Y1,
    XN is (X0+X1)/2,
    YN is (Y0+Y1)/2,
    findall(O, objectInQuadrant(b(X0,Y0,X1,Y1),[O]), Object),
    findall(NWO, objectInQuadrant(b(X0,Y0,XN,YN),[NWO]), NWObjects),
    findall(NEO, objectInQuadrant(b(XN,Y0,X1,YN),[NEO]), NEObjects),
    findall(SWO, objectInQuadrant(b(X0,YN,XN,Y1),[SWO]), SWObjects),
    findall(SEO, objectInQuadrant(b(XN,YN,X1,Y1),[SEO]), SEObjects),
    NW = node(b(X0,Y0,XN,YN),_,NWObjects),
    tree(NW),
    NE = node(b(XN,Y0,X1,YN),_,NEObjects),
    tree(NE),
    SW = node(b(X0,YN,XN,Y1),_,SWObjects),
    tree(SW),
    SE = node(b(XN,YN,X1,Y1),_,SEObjects),
    tree(SE).

    /*
     %mit Quadrantliste:
    tree(node(b(X0,Y0,X1,Y1),[],Object,Quadrant)) :- % empty Quadrant
      Empty = [],
      X0 < X1,
      Y0 < Y1,
      count_elements(Object,Count),
      (Count =:= 0;
      Count =:= 1,
      bounding_box(Object,OX0,OY0,OX1,OY1),
      boxes_overlap(OX0,OY0,OX1,OY1,X0,Y0,X1,Y1),
      append([b(X0,Y0,X1,Y1),Object],Empty,Quadrant)).

    tree(node(b(X0,Y0,X1,Y1),[NW,NE,SW,SE],Object,Quadrant)) :-
    X0 < X1,
    Y0 < Y1,
    XN is (X0+X1)/2,
    YN is (Y0+Y1)/2,
    %findall(O, objectInQuadrant(b(X0,Y0,X1,Y1),[O]), Object),
    count_elements(Object,Count),
    Count > 1,
    findall(NWO, objectInQuadrant(b(X0,Y0,XN,YN),[NWO]), NWObjects),
    findall(NEO, objectInQuadrant(b(XN,Y0,X1,YN),[NEO]), NEObjects),
    findall(SWO, objectInQuadrant(b(X0,YN,XN,Y1),[SWO]), SWObjects),
    findall(SEO, objectInQuadrant(b(XN,YN,X1,Y1),[SEO]), SEObjects),
    NW = node(b(X0,Y0,XN,YN),_,NWObjects,NWQuadrant),%NWObjects),
    tree(NW),
    %NEObjects = [],
    NE = node(b(XN,Y0,X1,YN),_,NEObjects,NEQuadrant),%NEObjects),
    tree(NE),
    %SWObjects = [],
    SW = node(b(X0,YN,XN,Y1),_,SWObjects,SWQuadrant),%SWObjects),
    tree(SW),
    %SEObjects = [],
    SE = node(b(XN,YN,X1,Y1),_,SEObjects,SEQuadrant),%SEObjects),
    tree(SE),
    append([],NWQuadrant,Quadrant),
    write('NWObjects'=NWObjects),
    write('NWObjects'=NEObjects),
    write('NWObjects'=SWObjects),
    write('NWObjects'=SEObjects),
    write('Quadranten'=Quadrant).*/
