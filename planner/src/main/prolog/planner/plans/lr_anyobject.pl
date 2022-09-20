:- module(plans_lr_anyobject, [target_any_object/5, plans_common:plan_last_resort/2, plans_common:plan_last_resort/3]).
:- use_module(planner(ab/objects)).
:- use_module(planner(geometric)).
:- use_module(planner(shot)).
:- use_module(common).
:- use_module(planner(data)).
:- use_module(library(yall)).


% Finds a plan for targetting a Pig, returns a list of a target, the strategy chosen, confidence
% plan_(-DecisionList)
plans_common:plan_last_resort(Bird, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"anyObjectLR", confidence:C, reasons:Pigs}) :-
	target_any_object(Bird, Target, UUID, C, Pigs),
	shot_params_dict(UUID, Shot, ImpactAngle).
plans_common:plan_last_resort(Bird, Target, plan{bird:Bird, shot:Shot, target_object:Target, impact_angle:ImpactAngle, strategy:"anyObjectLR", confidence:C, reasons:Pigs}) :-
	target_any_object(Bird, Target, UUID, C, Pigs),
	shot_params_dict(UUID, Shot, ImpactAngle).

target_any_object(_, Target, UUID, C, [free(Pig)]) :-
  isHittable(Target, UUID),
  \+ hill(Target),
  shape(Target, _,TX,TY,_,_),
  findall(
    [P,PigDistance],
    (
      pig(P,PX,PY,_,_),
      distance([TX, TY], [PX, PY], PigDistance)
    ),
    Distances
  ),
  min_member([[_,ADist],[_,BDist]]>>(ADist=<BDist), [Pig,MinDistance], Distances),
  C is 0.5 - MinDistance * 0.002.
  