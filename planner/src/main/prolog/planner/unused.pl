

%Fallback isOn/2 for legacy compatibility
%isOn(Object,hill) :-
%    isOn(Object,hill,_).


/*
penetrate(Bird, Target, Angle, Desc, Confidence, [Target]) :-
    (pig(Target) ; tnt(Target)),
    \+isHittable(Target,_),
    shape(Target, _, X, Y, _, _),
    shots_at_point(B, X, Y, SHOTS),
    member([_,_,_,_,_,Objects,TOF,RX,RY], SHOTS),
    penetration(Bird, Objects, P),
    (second_chance(Objects) -> Bonus is 0.2 ; Bonus is 0.0),
    P =< 1.5,
    ((P<1.0) -> Confidence is 1.0+Bonus ; (Confidence is Bonus + 1.0 - 0.33*P)).
*/


%nearby_pigs(Target, Pigs) :-
%    hasMaterial(Target, _, X, Y, _, _),
%    findall(P, (pig(P, PX, PY, _, _), sqrt((PX-X)^2 + (PY-Y)^2) < 50.0), Pigs).


% center of object
%center(XM, YM, X0, Y0, Width, Height) :-
%    XM is X0 + 0.5*Width,
%    YM is Y0 + 0.5*Height.

%direction(X0, Y0, X1, Y1, D) :-
%    A is atan2(Y1-Y0, X1-X0),
