:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

add_prolog_path :-
	prolog_load_context(file, File),
	file_directory_name(File, Dir),

	asserta(user:file_search_path(planner_home, Dir)).

:- add_prolog_path.

add_planner_path :-
  user:file_search_path(planner_home, Dir),
  atom_concat(Dir, '/src/main/prolog/planner', Planner),
	asserta(user:file_search_path(planner_main, Planner)).

:- add_planner_path.


add_behind_the_corner_path :-
  user:file_search_path(planner_home, Dir),
  atom_concat(Dir, '/behind_the_corner/build/libs', BTC),
  (exists_directory(BTC) ->
    asserta(user:file_search_path(lib, BTC));
    true
  ).
	
:- add_behind_the_corner_path.

add_test_path :-
  user:file_search_path(planner_home, Dir),
  atom_concat(Dir, '/src/test/prolog/planner', TestHome),
	asserta(user:file_search_path(planner_test, TestHome)).

:- add_test_path.

add_test_resources_path :-
  user:file_search_path(planner_home, Dir),
  atom_concat(Dir, '/src/test/resources', TestHome),
	asserta(user:file_search_path(planner_test_resources, TestHome)).

:- add_test_resources_path.