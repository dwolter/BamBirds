#ifndef _BEHIND_THE_CORNER_UTILS_H_INCLUDED
#define _BEHIND_THE_CORNER_UTILS_H_INCLUDED

#include <vector>
#include <tuple>
#include <cmath>
#include <boost/qvm/vec.hpp>

#define M_2_TIMES_PI 2 * M_PI
#define M_1_5_TIMES_PI (M_PI + M_PI_2)

#define FRICTION 0.4
#define RESTITUTION 0.4


// general utils

enum ZoneEdge {
    LEFT,
    BOTTOM,
    RIGHT,
    TOP
};

enum MovementType {
    FLY,
    BOUNCE,
    SLIDE,
    GOAL
};

enum IsHigh {
    IsHigh_Unknown = -1,
    IsHigh_False,
    IsHigh_True
};

using Coords = std::pair<float, float>;
using AngleRange = std::pair<float, float>;

bool inside_bounds(Coords coords, int width, int height, bool without_ground);
bool inside_bounds(Coords coords, int width, int height);

Coords neighbour(Coords coords, ZoneEdge edge);

float euclidean_2d_dist(Coords a, Coords b);

bool isclose(double a, double b, double abs_dist);


std::vector<Coords> bresenham(Coords a, Coords b);

template<typename T>
T swap_pair(std::pair<T,T> pair_to_swap);

// Matrix utils

template<typename T>
T getMatrixValue(std::vector<std::vector<T> > &matrix, Coords coords);

template<typename T>
void setMatrixValue(std::vector<std::vector<T> > &matrix, Coords coords, T value);

ZoneEdge cw(ZoneEdge cur);
ZoneEdge ccw(ZoneEdge cur);
ZoneEdge par(ZoneEdge cur);

float income_angle(ZoneEdge edge);
float bound_angle(float angle);
float bound_angle_with_negative(float angle);

AngleRange possible_angles(ZoneEdge from, ZoneEdge to);

Coords relative_point(Coords a, Coords b);

Coords edge_coordinates(Coords p, ZoneEdge e);

/**
 * @return The intersection of intervals a and b.
 * @throw Invalid argument exception of the intervals do not intersect
 */
AngleRange interval_intersection(AngleRange a, AngleRange b);

bool in_angle_interval(float a, AngleRange b);

bool angle_intervals_intersect(AngleRange a, AngleRange b);
bool angle_intervals_intersect(AngleRange a, AngleRange b, bool adapt_angles);

std::vector<std::pair<AngleRange,Coords>> merge_intervals(std::vector<std::pair<AngleRange, Coords>> intervals);

AngleRange to_angle_range(std::vector<float> angles);

float angle_interval_size(AngleRange a);

/**
 * @return True if the first angle is lower than the second, maximum distance is 180 degrees.
 */
bool correct_angle_order(AngleRange a);

/**
 * @return The minimum velocity to reach relative coordinates p
 */
float minimum_velocity(Coords p);

/**
 * @return Two Coordinates for the lower and higher bound of relative points. They are sorted by their maximally possible degree counterclockwise
 */
std::pair<Coords, Coords> get_calculation_points(Coords edge_point_0, Coords edge_point_new, ZoneEdge entry, ZoneEdge exit);

/**
 * @param is_high If the higher or lower option for the parabola should be used
 * @return the angles to hit the relative calculation points
 */
AngleRange angles_to_hit_edge(float velocity, std::pair<Coords, Coords> calculation_points, IsHigh is_high);

AngleRange calc_current_angles(AngleRange angles, float velocity, std::pair<Coords, Coords> calculation_points);

float calc_start_angle(Coords point, float angle, float velocity);

std::pair<float, float> calc_bounce(Coords edge_point, std::pair<float, float> start_angle_and_current_angle, float velocity, boost::qvm::vec<float,2> wall_normal_vector);

std::pair<AngleRange,float> calc_bounce(Coords edge_point, std::tuple<AngleRange, AngleRange> start_range_and_current_angles, float velocity, boost::qvm::vec<float,2> wall_normal_vector);

boost::qvm::vec<float,2> current_velocity(float x, float velocity, float theta_0);
float vector_angle_between(boost::qvm::vec<float,2> v1, boost::qvm::vec<float,2> v2);
#endif
