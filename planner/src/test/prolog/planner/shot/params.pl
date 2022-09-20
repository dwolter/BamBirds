:- use_module(planner_main(shot)).
:- use_module(planner_main(physics)).
:- use_module(planner_main(ab)).
:- use_module(planner_main(data)).

:- begin_tests(shot_params).

% shot_params should return the same results as would be calculated with the traditional shots_at_point
test(shot_params) :-
	main:load_data(planner_test_resources(example_situation1)),
  in_slingshot(Bird),
	shots_at_point(Bird, 500, 300, FullShots),
	shots_at_point(Bird, target, 500, 300, Angles),
  forall(
    member(Index, [0,1]),

    (
      nth0(Index, FullShots, [_, _, _, A1, B1, _, _, RX1, RY1]),
      nth0(Index, Angles, [UUID | _]),
      parabola(target, UUID, _, _, A2, B2),
      shot_params(UUID, _, _, RX2, RY2, _,_),
      assertion(A1 == A2),
      assertion(B1 == B2),
      assertion(RX1 == RX2),
      assertion(RY1 == RY2)
    )
  ).

:- end_tests(shot_params).