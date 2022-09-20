:- module(plans, [lookahead_plans/1, last_resort/1, exists_better_plan/3, exists_better_plan/4, plan_compare/3, remove_inferior_plans/3, rate_plans/3, attacked_pigs/2]).
:- use_module(plans/common, [plan/2, plan_last_resort/2, plan_last_resort/3, extract_targets_from_reasons/4, extract_all_targets_from_reasons/2]).
%:- use_module(plans/airbomb).
:- use_module(plans/white_bird).
:- use_module(plans/approximate_shot).
:- use_module(plans/black_bird).
:- use_module(plans/bunker).
:- use_module(plans/collapse_structure).
:- use_module(plans/defrost).
:- use_module(plans/domino).
:- use_module(plans/fly_by_bomb).
:- use_module(plans/heavy_object).
:- use_module(plans/lr_anyobject).
:- use_module(plans/lr_bounce).
:- use_module(plans/lr_domino).
:- use_module(plans/lr_rebound).
:- use_module(plans/lr_yellow_shot).
:- use_module(plans/target_pig).
:- use_module(plans/tnt).
:- use_module(shot/hittable).
:- use_module(shot/simple).
:- use_module(contrib/between_with_step).
:- use_module(ab).
:- use_module(data).
:- use_module(common/list).
:- use_module(library(http/json)). 

print_intermediate_plan(Plan) :-
	write('%'),
	json_write(current_output, Plan, [width(0),serialize_unknown(true)]),
	writeln(""),
	flush_output.

last_resort(LastResortPlans) :-
	in_slingshot(Bird),
	% FIXME: must be changed to a fall-off-the-wall-strategy!!    
%    findall([Target, Angle, Descriptor, C, P, ShotDesc]
%	    airbomb(Bird, Target, Angle, Descriptor, C, P, ShotDesc),
%	    Plans_Closeby),
	(setof(
			P,
			(
				plan_last_resort(Bird, P),
				print_intermediate_plan(P)
			),
			LastResortPlans
	); LastResortPlans=[]).


%% try adding last resort plans for attacking pigs not otherwise aimed at

pig_target(Target, Pigs) :-
	member(Target, Pigs).
pig_target(Target, Pigs) :-
	canExplode(Target, P),
	member(P, Pigs).

reason_intersection(Targets, Reasons, Intersection) :-
	extract_all_targets_from_reasons(Reasons, ReasonTargets),
	intersection(Targets, ReasonTargets, Intersection).

last_resort_pigs([],[]).
last_resort_pigs(Pigs, LastResortPlans) :-
	in_slingshot(Bird),
	(setof(
		Plan, 
		Pig^Reasons^(
			member(Pig, Pigs),
			plan_last_resort(Bird, Pig, Plan),
			get_dict(reasons, Plan, Reasons),
			\+ reason_intersection(Pigs, Reasons, []),
			print_intermediate_plan(Plan)
		), 
		LastResortPlans
	); LastResortPlans=[]), !.

% predicate for comparing plans, plans with high confidence
% killing many pigs go first
plan_compare(=, Plan1, Plan2) :- 
	Plan1.target_object = Plan2.target_object,
	Plan1.strategy = Plan2.strategy,
	Plan1.bird = Plan2.bird,
	Plan1.shot.uuid = Plan2.shot.uuid,
	Plan1.shot.tap_time = Plan2.shot.tap_time, !.
plan_compare(REL, Plan1, Plan2) :-
	length(Plan1.reasons,Reasons1Length),
	length(Plan2.reasons,Reasons2Length),
	((Plan1.confidence + 0.1*Reasons1Length =< Plan1.confidence + 0.1*Reasons2Length) -> REL = > ; REL = <).


% allPlans_(-ListOfPlans)
allPlans(SortedPlans) :-
	in_slingshot(Bird),
	findall(Plans, plan(Bird,Plans), AllPlans),
	predsort(plan_compare, AllPlans, SortedPlans), ! % remove duplicates
	.


exists_better_plan(Plan, ConfidenceDifference, OtherPlans) :- 
	exists_better_plan(Plan, ConfidenceDifference, OtherPlans, []).

%% check for existence of higher-confidence plan
exists_better_plan(_, _, [], _) :- false.
exists_better_plan(Plan, ConfidenceDifference, [PlanB | OtherPlans], Options) :-
	(
		(member(check_target(true), Options) -> Plan.target_object = PlanB.target_object; true),
		extract_targets_from_reasons(Plan.reasons, Destroy, Affect, Free),
		extract_targets_from_reasons(PlanB.reasons, BDestroy, BAffect, BFree),
		append([BDestroy, BAffect, BFree], AllBTargets),
		append([BDestroy, BAffect], BDestroyAffect),
		subset(Free, AllBTargets),
		subset(Affect, BDestroyAffect),
		subset(Destroy, BDestroy),
		PlanB.confidence > (Plan.confidence-ConfidenceDifference), !
	) -> true;
	exists_better_plan(Plan, ConfidenceDifference, OtherPlans, Options).


%% drop plans for which a more confident alternative exists
remove_inferior_plans([],_,[]).
remove_inferior_plans([Plan | Plans], AllPlans, RestPlans) :-
	(exists_better_plan(Plan, 0, AllPlans, [check_target(true)]) -> remove_inferior_plans(Plans, AllPlans, RestPlans) ;
	 (remove_inferior_plans(Plans, AllPlans, RestPlans2), RestPlans=[Plan|RestPlans2])).


all_bird_plans(Bird, Plans) :-
	in_slingshot(SlingBird),
	(setof( % remove duplicates
			P,
			(
				plan(Bird, P),
				(SlingBird == Bird -> print_intermediate_plan(P); true)
			),
			AllPlans
	); AllPlans=[]),
	remove_inferior_plans(AllPlans, AllPlans, Plans).

attacked_pigs([],[]).
attacked_pigs([Plan | Plans], AllAttackedPigs) :-
	attacked_pigs(Plans, AttackedPigs),
	(Plan.confidence > 0.49 ->
		(extract_all_targets_from_reasons(Plan.reasons,AllReasons),
		include(pig,AllReasons,Pigs));
		Pigs=[]
	),
	append(AttackedPigs, Pigs, MoreAttackedPigs),
	list_to_set(MoreAttackedPigs, AllAttackedPigs).

unattacked_pigs(Pigs, Attacked) :-
	findall(P, pig(P), AllPigs),
	subtract(AllPigs, Attacked, Pigs).

%%
%% rate_plans changes confidence of plans according to available alternatives
%% we need to increase confidence in plans within CurrentPlans which are significantly
%% better than alternatives to come (e.g., use a yellow bird to destroy a wooden shelter,
%% avoid forthcoming red birds).

rate_plans([], _, []).
rate_plans(CurrentPlans, [], CurrentPlans).
rate_plans([Plan | CurrentPlans], OtherPlans, [NewPlan | RatedCurrentPlans ]) :-
	(exists_better_plan(Plan, 0.15, OtherPlans, []) -> NewConfidence is Plan.confidence ; NewConfidence is Plan.confidence + 1),
	NewPlan = Plan.put(confidence, NewConfidence),
	rate_plans(CurrentPlans, OtherPlans, RatedCurrentPlans).

other_bird_plans(CurrentBird, CurrentPlans, Other_Plans) :-
	setof(BC,B^(bird(B), B \= CurrentBird, hasColor(B,BC)),BCs),
	findall(Plans, 
		(
			member(BC, BCs),
			once(hasColor(Bird,BC)),
			(hasColor(CurrentBird, BC) -> 
				Plans=CurrentPlans;
				all_bird_plans(Bird,Plans)
			)
		), 
		BirdPlans
	),
	append_lists(BirdPlans, Other_Plans).

lookahead_plans(LPlans) :-
	birdOrder(_,1), % more than one bird left
	in_slingshot(Bird),
	all_bird_plans(Bird,Greedy_Plans),
	other_bird_plans(Bird, Greedy_Plans, Other_Plans),
	rate_plans(Greedy_Plans, Other_Plans, Plans),
	attacked_pigs(Plans, PigsAttackedByPlan),
	unattacked_pigs(Pigs, PigsAttackedByPlan),
	last_resort_pigs(Pigs, LastResortPlans),
	append_lists([Plans, LastResortPlans], FinalPlans),
	predsort(plan_compare, FinalPlans, SPlans), !, % remove duplicates to improve parsing in Java (regular expressions are slow!)
	(SPlans\=[] ->
 		LPlans=SPlans;
		last_resort(LPlans)
	).

lookahead_plans(Plans) :-
	\+ birdOrder(_,1), % single bird left
	in_slingshot(B),
	all_bird_plans(B,PS), !,
	predsort(plan_compare, PS, SortedPlans), !, % remove duplicates to improve parsing in Java (regular expressions are slow!)
	attacked_pigs(SortedPlans, PigsAttackedByPlan),
	unattacked_pigs(Pigs, PigsAttackedByPlan),
	last_resort_pigs(Pigs, LPlans),
	append(LPlans, SortedPlans, AllPlans),
	(AllPlans \= [] -> Plans=AllPlans ; last_resort(Plans)).
