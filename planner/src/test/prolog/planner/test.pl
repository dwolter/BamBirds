:- module(planner_test, []).

% Loads all tests in the current directory an subdirectories
% Only files with the extensions `pl` and `plt`
load_all_tests :-
	prolog_load_context(file, File),
	file_directory_name(File, Dir),
    foreach(
        directory_member(Dir, Entry, options{recursive:true, extenstions:[pl, plt], exclude: "test.pl"}),
        (exists_file(Entry) ->
            consult(Entry);
            true
        )
    ).

:- load_all_tests.