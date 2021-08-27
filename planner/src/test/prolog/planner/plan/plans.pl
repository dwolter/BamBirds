:- use_module(planner_main(plans)).

:- begin_tests(exists_better_plan).

test(exists_better_plan_true) :-
  Plan = plan{confidence:0.5,impact_angle:1,reasons:[pig],target_object:pig},
  PlanList = [plan{confidence:0.5,impact_angle:1,reasons:[pig],target_object:pig},plan{confidence:0.7,impact_angle:1,reasons:[pig],target_object:pig}],
  assertion(exists_better_plan(Plan, PlanList)).

test(exists_better_plan_false) :-
  Plan = plan{confidence:0.5,impact_angle:1,reasons:[],target_object:pig},
  PlanList = [plan{confidence:0.5,impact_angle:1,reasons:[],target_object:pig},plan{confidence:0.3,impact_angle:1,reasons:[],target_object:pig}],
  assertion(\+ exists_better_plan(Plan, PlanList)).

:- end_tests(exists_better_plan).

:- begin_tests(alternative_plan).

test(alternative_plan_exists) :-
  Plan = plan{confidence:0.5,impact_angle:1,reasons:[pig],target_object:pig},
  PlanList = [
    plan{confidence:0.5,impact_angle:1,reasons:[],target_object:pig},
    plan{confidence:0.7,impact_angle:1,reasons:[pig],target_object:pig}, 
    plan{confidence:0.5,impact_angle:1,reasons:[pig],target_object:pig}],
  assertion(alternative_plan(Plan, PlanList)).

test(alternative_plan_does_not_exist) :-
  Plan = plan{confidence:0.5,impact_angle:1,reasons:[pig1],target_object:pig},
  PlanList = [plan{confidence:0.3,impact_angle:1,reasons:[pig1],target_object:pig},plan{confidence:0.5,impact_angle:1,reasons:[],target_object:pig}],
  assertion(\+ alternative_plan(Plan, PlanList)).

:- end_tests(alternative_plan).

:- begin_tests(attacked_pigs).

test(attacked_pigs) :-
  PlanList = [plan{reasons:[pig1]},plan{reasons:[pig2,pig1]}],
  attacked_pigs(PlanList, Pigs),
  assertion(subtract(Pigs, [pig1,pig2], [])).

:- end_tests(attacked_pigs).
