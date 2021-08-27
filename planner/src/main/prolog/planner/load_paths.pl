:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

add_prolog_path :-
	prolog_load_context(file, File),
	file_directory_name(File, Dir),

	asserta(user:file_search_path(prolog_home, Dir)).

:- add_prolog_path.

add_planner_path :-
	user:file_search_path(prolog_home, Dir),
	asserta(user:file_search_path(planner, Dir)).

:- add_planner_path.