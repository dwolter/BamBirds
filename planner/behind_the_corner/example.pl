:- load_foreign_library(build/libs/behind_the_corner).

run_1(Iter, EdgeLength, AngleRange) :-
    behind_the_corner:load_scene('resources/scene-representation-1-rep', Handle),
    behind_the_corner:simulate(Handle, _{
        start: [297,331],
        target: [522,376],
        target_type:9,
        target_size:20,
        angle_range:AngleRange,
        velocity: 25,
        gravity: 1,
        edge_length:EdgeLength,
        maxiter: Iter
        }, R),
    write_ln(R),
    behind_the_corner:free_scene(Handle).


run_2(Iter, EdgeLength, AngleRange) :-
    behind_the_corner:load_scene('resources/scene-representation-2-rep', Handle),
    behind_the_corner:simulate(Handle, _{
        start: [247,332],
        target: [571,331],
        target_type:9,
        target_size:20,
        angle_range:AngleRange,
        velocity: 70,
        gravity: 1,
        edge_length:EdgeLength,
        maxiter: Iter
        }, R),
    write_ln(R),
    behind_the_corner:free_scene(Handle).


run_4(Iter, EdgeLength, AngleRange) :-
    behind_the_corner:load_scene('resources/scene-representation-4-rep', Handle),
    behind_the_corner:simulate(Handle, _{
        start: [131,336],
        target: [238,352],
        target_type:9,
        target_size:20,
        angle_range:AngleRange,
        velocity: 70,
        gravity: 1,
        edge_length:EdgeLength,
        maxiter: Iter
        }, R),
    write_ln(R),
    behind_the_corner:free_scene(Handle).
