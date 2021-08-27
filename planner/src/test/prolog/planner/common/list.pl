:- use_module(planner_main(common/list)).

:- begin_tests(prune_list).

test(prune_list) :-
        prune_list([a,b,c,d], 2, A),
        assertion(length(A, 2)),
        assertion(member(b, A)), !.

:- end_tests(prune_list).