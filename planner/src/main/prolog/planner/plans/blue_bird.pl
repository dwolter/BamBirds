:- module(plans_blue_bird, [secondary_goal_delta/5]).
:- use_module(planner(data)).

%%%
%%% secondary goals for blue birds
%%%

secondary_parabola(A, B, X, A2, B2) :-
	scene_scale(S,_),
	D is (2*A*X + B),
	CA is (cos(pi/11.0)),
	SA is (sin(pi/11.0)),
	Vh is (CA - SA*D),
	Vv is (SA - CA*D),
	A2 is (-1 / (2 * S * Vh*Vh)),
	Vh \= 0,
	B2 is (Vv / Vh).

secondary_goal_delta(A, B, X, Target, Delta) :-
	shape(Target,_,TX,TY,_,_),
	slingPivot(X0, Y0),
	secondary_parabola(A,B, X, A2, B2),
	DX is (TX - X0 - X),
	Delta is (TY - (Y0 - A*X*X - B*X - A2*DX*DX - B2*DX)).