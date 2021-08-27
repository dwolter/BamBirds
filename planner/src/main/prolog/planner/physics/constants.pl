:- module(physics_constants, [
	low_angle_change/1,
	low_angle_velocity/1,
	high_angle_begin/2,
	high_angle_change/2,
	high_angle_velocity/2
	]).

% constants for *new* vision module
low_angle_change([ -0.0230, -7.871e-4, 0.0540]).
low_angle_velocity([0.0473, -0.1756,   2.8654]).

high_angle_begin(blue, 1.347847968145141). % 77.226 degrees
high_angle_begin(red, 1.2998514137152968). % 74.476
high_angle_begin(yellow, 1.3095554443563853).
high_angle_begin(black, 1.2701982697239131).
high_angle_begin(white, 1.2106476356458666).

high_angle_change(blue, [-6.2164, 17.7277, -12.5889]).
high_angle_change(red,  [-6.8544, 19.1149, -13.2502]).
high_angle_change(yellow, [-7.1737, 20.0922, -13.9949]).
high_angle_change(black, [-10.4124, 28.4201, -19.2827]).
high_angle_change(white, [-11.8720, 31.2441, -20.4036]).

high_angle_velocity(blue, [39.1345, -119.2619, 92.3808]).
high_angle_velocity(red, [42.1667, -124.9211, 93.8631]).
high_angle_velocity(yellow, [43.7502, -130.3646, 98.4179]).
high_angle_velocity(black,  [65.4336, -186.8649, 134.515]).
high_angle_velocity(white, [73.2264, -199.8274, 137.3380]).
