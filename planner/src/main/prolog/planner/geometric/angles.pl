:- module(geometric_angles, [angleToRadiant/2, radiantToAngle/2]).


angleToRadiant(Angle , Radiant) :-
	Radiant is (Angle * pi / 180.0).

radiantToAngle(Radiant, Angle) :-
	Angle is (Radiant * 180.0) / pi.