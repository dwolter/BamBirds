# Planner

## Java part

The java part is currently mostly a wrapper for Prolog. It creates the Knowledge file and parses the results.

## Prolog part

The Prolog part of the planner is structured into multiple modules for a more clear understanding of what happens when.
They are all located in `src/main/prolog/planner`. For each module there is a folder containing the module files, and a 
top-level module that reexports all modules in the directory.  
 
### Run the planner

To start the planner immediately you can run
```bash
$ swipl start.pl
```
and then enter the path to your situation file.

For a more specific use case you can run this:
```shell
$ swipl main.pl
```
```ijprolog
1 ?- load_data("path/to/situation/file"). % Load data without prompt
2 ?- initiate_planner(AllPlans). % Run the planner	
3 ?- initiate_planner(AllPlans), export_tikz(AllPlans). % Run planner and then export the plans with latex
```

Because of the module structure you will sometimes see something like this:
```
Correct to: "tikz:export_tikz(AllPlans)"?
```
Just enter `y` and everything will be fine

### Tests

To run the test you can run either of these:
```bash
$ swipl test.pl
$ ../gradlew testProlog # Basically just a wrapper for gradle around the line above
```

The tests for prolog are located in `src/test/prolog/planner`. 
An example unit test is in [list.pl](src/test/prolog/planner/common/list.pl), integration tests with situation file in [integration.pl](src/test/prolog/planner/integration.pl).

To add a new test just create a file in the folder `src/test/prolog/planner/[your_module]` with the possible extension `.pl` or `.plt` and it will be executed.

If you want to use situation files add them to `src/test/resources/` and load them like this:

```ijprolog
:- begin_tests(example).

test(example) :-
	main:load_data(planner_test_resources(path/to/file/in/resources)),
	% execute your tests here
	assertion(true),!.

:- end_tests(example).
```
