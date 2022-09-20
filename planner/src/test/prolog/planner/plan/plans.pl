:- use_module(planner_main(plans)).

:- begin_tests(exists_better_plan).

test(exists_better_plan_true) :-
  Plan = plan{confidence:0.5,reasons:[destroy(pig)],target_object:pig},
  PlanList = [plan{confidence:0.5,reasons:[destroy(pig)],target_object:pig},plan{confidence:0.7,reasons:[destroy(pig)],target_object:pig}],
  assertion(exists_better_plan(Plan, 0, PlanList)).

test(exists_better_plan_false) :-
  Plan = plan{confidence:0.5,reasons:[],target_object:pig},
  PlanList = [plan{confidence:0.5,reasons:[],target_object:pig},plan{confidence:0.3,reasons:[],target_object:pig}],
  assertion(\+ exists_better_plan(Plan, 0, PlanList)).

test(exists_better_plan_exists) :-
  Plan = plan{confidence:0.5,reasons:[destroy(pig)],target_object:pig},
  PlanList = [
    plan{confidence:0.5,reasons:[],target_object:pig},
    plan{confidence:0.7,reasons:[destroy(pig)],target_object:pig}, 
    plan{confidence:0.5,reasons:[destroy(pig)],target_object:pig}],
  assertion(exists_better_plan(Plan, 0.15, PlanList)).

test(exists_better_plan_does_not_exist) :-
  Plan = plan{confidence:0.5,reasons:[destroy(pig1)],target_object:pig},
  PlanList = [plan{confidence:0.3,reasons:[destroy(pig1)],target_object:pig},plan{confidence:0.5,reasons:[],target_object:pig}],
  assertion(\+ exists_better_plan(Plan, 0.15, PlanList)).

:- end_tests(exists_better_plan).

:- begin_tests(attacked_pigs).

test(attacked_pigs) :-
  PlanList = [plan{reasons:[destroy(pig1)], confidence: 0.9},plan{reasons:[destroy(pig2),destroy(pig1)], confidence: 0.9}],
  attacked_pigs(PlanList, Pigs),
  assertion(subtract(Pigs, [pig1,pig2], [])).

:- end_tests(attacked_pigs).
