:- module(test, []).

:- [load_paths].

:- use_module(planner_main(main)).
:- use_module(planner_test(test)).

:- (run_tests -> halt; halt(1)).
