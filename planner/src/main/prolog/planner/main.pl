:- module(main, [
    main/0,
    demo_main/0,
    easyMain/0,
    initiate_planner/1,
		load_data/1,
		load_data/0
]).
:- [load_paths].
:- use_module(plans).
:- use_module(shot/collision_shapes).
:- use_module(shot/hittable).
:- use_module(common/list).
:- use_module(tikz).
:- use_module(data).
:- use_module(library(http/json)).
:- use_module(library(prolog_stack)).
:- reexport(data).

%% 2DO
%% - schwarzer vogel nahe an schwein bringen, auch wenn Hill dazwischen
%% - Turm anstossen, wenn schweine nahe am rand liegen und herunterkullern
%%   koennen
%% - Eis-Stütze in Level 10 (höhere Konfidenz bei 'einmaligen' chancen)
%% - konfidenz boosten, wenn mehrere gründe für ein Target sprechen

%% Entrance point for Java (SWIConnector class)

load_data :-
	read(Filename),
	catch(load_data(Filename),Error,(print_message(error,Error), load_data)).

prepare :-
	shot_collision_shapes:purge_collision_shapes,
	abolish_all_tables,
	((generate_collision_shapes, assert_parabolas_and_hittable) -> true; true).


initiate_planner(AllPlans) :-
	prepare,
	lookahead_plans(AllPlans).
	% findall(P, shoot_test(P), AllPlans),

main :-
	catch_with_backtrace(
		(getenv('DEMO_MODE', 'true') -> 
			demo_main;
			(
				load_data,
				initiate_planner(AllPlans),
				json_write(current_output, AllPlans, [width(0)]),
				write_ln(""),
				flush_output()
			)
		),
		Error,
		(
			writeln(user_error, 'Failed to complete planning, the following error occured:'),
			print_message(error, Error),
			writeln(user_error, 'Exiting because there is no way to recover safely')
		)
	),
	halt.

demo_main :-
	setenv('CONVERT_ENABLE', true),
	load_data,
	write_tikz(),
	initiate_planner(AllPlans),
	prune_list(AllPlans,5,FilteredPlans),
	write_tikz(FilteredPlans),
	json_write(current_output, AllPlans, [width(0)]),
	write_ln(""),
	flush_output(),
	halt.

%% for compatibility
easyMain :-
	main.
