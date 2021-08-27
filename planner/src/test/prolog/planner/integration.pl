:- use_module(planner_main(main)).

:- begin_tests(situation1).

test(situation1) :-
	main:load_data(planner_test_resources(example_situation1)),
	main:initiate_planner(Plans),
	length(Plans, Length),
	assertion(Length > 1),!.

:- end_tests(situation1).

:- begin_tests(situation2).

test(situation2) :-
	main:load_data(planner_test_resources(example_situation2)),
	main:initiate_planner(Plans),
	length(Plans, Length),
	assertion(Length > 1),!.

:- end_tests(situation2).