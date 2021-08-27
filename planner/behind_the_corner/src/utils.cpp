#include <vector>
#include <tuple>
#include <iostream>
#include "utils.h"
#include <cmath>
#include <boost/qvm/vec_operations.hpp>
#include <boost/qvm/vec_access.hpp>
#include <stdexcept>
#include <algorithm>
#include <map>

using namespace boost::qvm;

template <typename T>
T getMatrixValue(std::vector<std::vector<T>> matrix, Coords coords)
{
    return matrix.at(std::get<0>(coords)).at(std::get<1>(coords));
}

template <typename T>
void setMatrixValue(std::vector<std::vector<T>> matrix, Coords coords, T value)
{
    matrix.at(std::get<0>(coords)).at(std::get<1>(coords)) = value;
}

template <typename T>
void swap_pair(std::pair<T, T> *pair_to_swap)
{
    T tmp = pair_to_swap->first;
    pair_to_swap->first = pair_to_swap->second;
    pair_to_swap->second = tmp;
}

bool inside_bounds(Coords coords, int width, int height, bool without_ground_or_height)
{
    return coords.first >= 0 &&
           coords.first < width &&
           (without_ground_or_height ||
            (coords.second >= 0 &&
             coords.second < height));
}
bool inside_bounds(Coords coords, int width, int height)
{
    return inside_bounds(coords, width, height, false);
}

bool isclose(double a, double b, double abs_dist)
{
    return std::fabs(a - b) < abs_dist;
}

float euclidean_2d_dist(Coords a, Coords b)
{
    float d1 = std::fabs(a.first - b.first);
    float d2 = std::fabs(a.second - b.second);

    return std::sqrt(d1 * d1 + d2 * d2);
}

std::vector<Coords> bresenham(Coords a, Coords b)
{
    int xa = a.first;
    int xb = b.first;
    int ya = a.second;
    int yb = b.second;
    int dx = std::abs(xa - xb), dy = abs(ya - yb);
    int p = 2 * dy - dx;
    int twoDy = 2 * dy, twoDyDx = 2 * (dy - dx);
    int x, y, xEnd;
    /*Determine which points to start and End */
    if (xa > xb)
    {
        x = xb;
        y = yb;
        xEnd = xa;
    }
    else
    {
        x = xa;
        y = ya;
        xEnd = xb;
    }
    std::vector<Coords> result;
    result.push_back(Coords(x, y));
    while (x < xEnd)
    {
        x++;
        if (p < 0)
        {
            p = p + twoDy;
        }
        else
        {
            y++;
            p = p + twoDyDx;
        }
        result.push_back(Coords(x, y));
    }
    return result;
}

ZoneEdge cw(ZoneEdge cur)
{
    return ZoneEdge((cur + 1) % 4);
}
ZoneEdge ccw(ZoneEdge cur)
{
    return ZoneEdge((cur + 3) % 4);
}
ZoneEdge par(ZoneEdge cur)
{
    return ZoneEdge((cur + 2) % 4);
}

Coords neighbour(Coords coords, ZoneEdge edge)
{
    switch (edge)
    {
    case TOP:
        return Coords(coords.first, coords.second + 1);
    case RIGHT:
        return Coords(coords.first + 1, coords.second);
    case BOTTOM:
        return Coords(coords.first, coords.second - 1);
    default:
    case LEFT:
        return Coords(coords.first - 1, coords.second);
    }
}

float bound_angle(float angle)
{
    while (angle < 0)
        angle += M_2_TIMES_PI;
    return fmod(angle, M_2_TIMES_PI);
}

float bound_angle_with_negative(float angle)
{
    angle = bound_angle(angle);
    return angle > M_PI ? angle - M_2_TIMES_PI : angle;
}

float angle_diff(float a, float b, bool ordered, bool bound, bool is_signed)
{
    if (ordered && b < a)
    {
        a -= M_2_TIMES_PI;
    }
    float angle = b - a;
    if (!is_signed)
    {
        angle = abs(angle);
    }
    if (bound)
    {
        angle = bound_angle(angle);
    }
    return angle;
}

float income_angle(ZoneEdge edge)
{
    return bound_angle(edge * M_PI_2);
}

AngleRange possible_angles(ZoneEdge from, ZoneEdge to)
{
    float start = income_angle(from);
    if (to == cw(from))
    {
        return AngleRange(bound_angle(start - M_PI_2), start);
    }
    else if (to == par(from))
    {
        return AngleRange(bound_angle(start - M_PI_4), bound_angle(start + M_PI_4));
    }
    else if (to == ccw(from))
    {
        return AngleRange(start, bound_angle(start + M_PI_2));
    }
    else
    {
        return AngleRange(bound_angle(start + M_PI_2), bound_angle(start - M_PI_2));
    }
}

Coords edge_coordinates(Coords p, ZoneEdge e)
{
    switch (e)
    {
    case TOP:
        return Coords(p.first, p.second + 0.5);
    case RIGHT:
        return Coords(p.first + 0.5, p.second);
    case BOTTOM:
        return Coords(p.first, p.second - 0.5);
    case LEFT:
    default:
        return Coords(p.first - 0.5, p.second);
        break;
    }
}

Coords relative_point(Coords a, Coords b)
{
    return Coords(b.first - a.first, b.second - a.second);
}

std::pair<AngleRange, AngleRange> angle_intervals_closer(AngleRange a, AngleRange b)
{

    float alpha_1 = a.first;
    float beta_1 = b.first;
    float alpha_2 = a.second < alpha_1 ? a.second + M_2_TIMES_PI : a.second;
    float beta_2 = b.second < beta_1 ? b.second + M_2_TIMES_PI : b.second;

    if (beta_2 < alpha_1)
    {
        beta_1 += M_2_TIMES_PI;
        beta_2 += M_2_TIMES_PI;
    }
    else if (beta_1 > alpha_2)
    {
        alpha_1 += M_2_TIMES_PI;
        alpha_2 += M_2_TIMES_PI;
    }

    return std::pair<AngleRange,AngleRange>(AngleRange(alpha_1, alpha_2), AngleRange(beta_1, beta_2));
}
bool angle_intervals_intersect(AngleRange a, AngleRange b)
{
    return angle_intervals_intersect(a, b, false);
}
bool angle_intervals_intersect(AngleRange a, AngleRange b, bool adapt_angles)
{
    if (adapt_angles)
    {
        auto closer = angle_intervals_closer(a, b);
        a = closer.first;
        b = closer.second;
    }
    return !(a.first > b.second || b.first > a.second);
}

float angle_interval_size(AngleRange a)
{
    return angle_diff(a.first,a.second,true,true,false);
}

AngleRange interval_intersection(AngleRange a, AngleRange b)
{
    // std::cout << "a: " << a.first << "," << a.second  << " b: " << b.first << "," << b.second << std::endl;
    auto closer = angle_intervals_closer(a, b);
    a = closer.first;
    b = closer.second;
    // std::cout << "closer\n" << "a: " << a.first << "," << a.second  << " b: " << b.first << "," << b.second << std::endl;

    if (!angle_intervals_intersect(a, b))
    {
        throw std::invalid_argument("Angle intervals do not intersect");
    }

    float theta_1 = std::max(a.first, b.first);
    float theta_2 = std::min(a.second, b.second);

    // std::cout << "Θ: " << Θ_1 << "," << Θ_2 << std::endl;

    return AngleRange(bound_angle(theta_1), bound_angle(theta_2));
}

bool in_angle_interval(float a, AngleRange b)
{
    float b_1 = b.first;
    float b_2 = b.second < b_1 ? b.second + M_2_TIMES_PI : b.second;
    return b_1 <= a && a <= b_2;
}

bool correct_angle_order(AngleRange a)
{
    float diff = a.second - a.first;
    if (std::abs(diff) > M_PI)
    {
        float a_0 = a.first > M_PI ? a.first - 2 * M_PI : a.first;
        float a_1 = a.second > M_PI ? a.second - 2 * M_PI : a.second;
        diff = a_1 - a_0;
    }
    return diff > 0;
}

AngleRange to_angle_range(std::vector<float> angles)
{
    for (int i = 0; i < angles.size(); i++)
    {
        for (int j = 0; j < angles.size(); j++)
        {
            if (i != j && std::abs(angles[i] - angles[j]) >= M_PI)
            {
                if (angles[i] > angles[j])
                {
                    angles[i] -= M_2_TIMES_PI;
                    break;
                }
                else
                {
                    angles[j] -= M_2_TIMES_PI;
                }
            }
        }
    }
    return {
        bound_angle(*std::min_element(angles.begin(), angles.end())),
        bound_angle(*std::max_element(angles.begin(), angles.end()))};
}

std::vector<std::pair<AngleRange,Coords>> merge_intervals(std::vector<std::pair<AngleRange, Coords>> intervals)
{
    std::vector<std::pair<AngleRange,Coords>> result;
    std::map<Coords,AngleRange> angle_map;
    for (auto interval : intervals)
    {
        try
        {
            auto current_interval = angle_map.at(interval.second);
            auto new_interval = to_angle_range({interval.first.first, interval.first.second, current_interval.first, current_interval.second});
            angle_map.at(interval.second) = new_interval;
        }
        catch(const std::out_of_range& e)
        {
            angle_map.emplace(interval.second, interval.first);
        }
    }
    for (auto pair : angle_map)
    {
        result.push_back({pair.second, pair.first});
    }
    return result;
}


std::pair<Coords, Coords> get_calculation_points(Coords edge_point_0, Coords edge_point_new, ZoneEdge entry, ZoneEdge exit)
{
    std::pair<Coords, Coords> result;
    Coords rel_point(edge_point_new.first - edge_point_0.first, edge_point_new.second - edge_point_0.second);
    switch (exit)
    {
    case RIGHT:
    case LEFT:
        result.first.first = rel_point.first;
        result.first.second = rel_point.second - std::copysign(0.5, rel_point.first);

        result.second.first = rel_point.first;
        result.second.second = rel_point.second + std::copysign(0.5, rel_point.first);
        // if (exit == LEFT || exit == RIGHT)
        // {
        //     result.first.first = edge_point_new.first - edge_point_0.first;
        //     result.first.second = edge_point_new.second - edge_point_0.second - 1;

        //     result.second.first = edge_point_new.first - edge_point_0.first;
        //     result.second.second = edge_point_new.second - edge_point_0.second + 1;
        // }
        // else if (exit == TOP)
        // {
        //     result.first.first = edge_point_new.first + 0.5 - edge_point_0.first;
        //     result.first.second = edge_point_new.second - edge_point_0.second - 0.5;

        //     result.second.first = edge_point_new.first - 0.5 - edge_point_0.first;
        //     result.second.second = edge_point_new.second - edge_point_0.second + 0.5;
        // }
        // else if (exit == BOTTOM)
        // {
        //     result.first.first = edge_point_new.first - 0.5 - edge_point_0.first;
        //     result.first.second = edge_point_new.second - edge_point_0.second - 0.5;

        //     result.second.first = edge_point_new.first + 0.5 - edge_point_0.first;
        //     result.second.second = edge_point_new.second - edge_point_0.second + 0.5;
        // }
        // if (entry == RIGHT)
        // {
        //     Coords tmp = result.first;
        //     result.first = result.second;
        //     result.second = tmp;
        // }
        break;
    case BOTTOM:
    case TOP:
        result.first.first = rel_point.first + 0.5;
        result.first.second = rel_point.second;

        result.second.first = rel_point.first - 0.5;
        result.second.second = rel_point.second;
        // if (exit == BOTTOM || exit == TOP)
        // {
        //     result.first.first = edge_point_new.first - edge_point_0.first - 1;
        //     result.first.second = edge_point_new.second - edge_point_0.second;

        //     result.second.first = edge_point_new.first - edge_point_0.first + 1;
        //     result.second.second = edge_point_new.second - edge_point_0.second;
        // }
        // else if (exit == LEFT)
        // {
        //     result.first.first = edge_point_new.first - edge_point_0.first - 0.5;
        //     result.first.second = edge_point_new.second - 0.5 - edge_point_0.second;

        //     result.second.first = edge_point_new.first - edge_point_0.first + 0.5;
        //     result.second.second = edge_point_new.second + 0.5 - edge_point_0.second;
        // }
        // else if (exit == RIGHT)
        // {
        //     result.first.first = edge_point_new.first - edge_point_0.first - 0.5;
        //     result.first.second = edge_point_new.second + 0.5 - edge_point_0.second;

        //     result.second.first = edge_point_new.first - edge_point_0.first + 0.5;
        //     result.second.second = edge_point_new.second - 0.5 - edge_point_0.second;
        // }
        // if (entry == BOTTOM)
        // {
        //     Coords tmp = result.first;
        //     result.first = result.second;
        //     result.second = tmp;
        // }
        break;
    default:
        throw std::invalid_argument("Unknown ZoneEdge " + std::to_string(exit));
    }
    return result;
}

float minimum_velocity(Coords p)
{
    return std::sqrt(
        p.second + std::sqrt(p.second * p.second + p.first * p.first));
}

float angle_to_hit(float v_0, Coords p, IsHigh is_high)
{
    if (isclose(p.first, 0, 1e-7))
    {
        if (is_high == IsHigh_True || p.second > 0)
            return M_PI_2;
        else
            return M_PI + M_PI_2;
    }
    float degree_adjust = p.first < 0 ? M_PI : 0;
    int i = is_high == IsHigh_True ? 1 : -1;
    float v_0_2 = v_0 * v_0;
    float v_0_4 = v_0_2 * v_0_2;
    float root = sqrt(v_0_4 - (p.first * p.first + 2 * p.second * v_0_2));
    if (isnan(root)) {
        // Since we should never be here if the velocity is not high enough, set root to 0
        root = 0;
    }
    return bound_angle(
        atan(
            (v_0_2 + i * root) /
            p.first) -
        degree_adjust);
}

double round_up(double value, int decimal_places)
{
    const double multiplier = std::pow(10.0, decimal_places);
    return std::ceil(value * multiplier) / multiplier;
}

double round_down(double value, int decimal_places)
{
    const double multiplier = std::pow(10.0, decimal_places);
    return std::floor(value * multiplier) / multiplier;
}

double round(double value, int decimal_places)
{
    const double multiplier = std::pow(10.0, decimal_places);
    return std::round(value * multiplier) / multiplier;
}

float maximum_x(float y, float velocity)
{
    float v_2 = velocity * velocity;
    return round_down(std::sqrt(v_2 * v_2 - 2 * y * v_2), 5);
}

float maximum_y(float x, float velocity)
{
    float v_2 = velocity * velocity;
    return round_down((v_2 * v_2 - x*x) / (2 * v_2), 5);
}

bool in_interval(int v, int i1, int i2)
{
    v = round(v, 7);
    if (i1 > i2)
    {
        return i2 <= v and v <= i1;
    }
    else
    {
        return i1 <= v and v <= i2;
    }
}

AngleRange angles_to_hit_edge(float velocity, std::pair<Coords, Coords> calculation_points, IsHigh is_high)
{
    AngleRange angles;
    if (calculation_points.first.first == calculation_points.second.first)
    { // vertical edge
        if (minimum_velocity(calculation_points.first) > velocity)
        {
            float new_y = maximum_y(calculation_points.first.first, velocity);
            if (in_interval(new_y, calculation_points.first.second, calculation_points.second.second))
            {
                calculation_points.first.second = new_y;
            }
            else
            {
                throw std::invalid_argument("Velocity not high enough");
            }
        }
        if (minimum_velocity(calculation_points.second) > velocity)
        {
            float new_y = maximum_y(calculation_points.second.first, velocity);
            if (in_interval(new_y, calculation_points.first.second, calculation_points.second.second))
            {
                calculation_points.second.second = new_y;
            }
            else
            {
                throw std::invalid_argument("Velocity not high enough");
            }
        }
    }
    else if (calculation_points.first.second == calculation_points.second.second)
    {
        int min_x = std::min(calculation_points.first.first, calculation_points.second.first);
        int max_x = std::max(calculation_points.first.first, calculation_points.second.first);
        if (minimum_velocity(calculation_points.first) > velocity)
        {
            float new_x = std::copysign(maximum_x(calculation_points.first.second, velocity), calculation_points.first.first);
            if (in_interval(new_x, min_x, max_x))
            {
                calculation_points.first.first = new_x;
            }
            else
            {
                throw std::invalid_argument("Velocity not high enough");
            }
        }
        if (minimum_velocity(calculation_points.second) > velocity)
        {
            float new_x = std::copysign(maximum_x(calculation_points.second.second, velocity), calculation_points.second.first);
            if (in_interval(new_x, min_x, max_x))
            {
                calculation_points.second.first = new_x;
            }
            else
            {
                throw std::invalid_argument("Velocity not high enough");
            }
        }
    }
    angles.first = angle_to_hit(velocity, calculation_points.first, is_high);
    angles.second = angle_to_hit(velocity, calculation_points.second, is_high);
    if (!correct_angle_order(angles))
    {
        swap_pair(&angles);
    }
    return angles;
}

float horizontal_direction(float angle)
{
    return std::cos(angle);
}

float current_angle(float theta_0, float velocity, Coords current_point)
{
    if (isclose(theta_0, M_PI_2, 1e-7))
    {
        if (minimum_velocity(current_point) < velocity)
        {
            return M_PI_2;
        }
        else
        {
            return M_1_5_TIMES_PI;
        }
    }
    else if (isclose(theta_0, M_1_5_TIMES_PI, 1e-7))
    {
        return M_1_5_TIMES_PI;
    }
    else
    {
        float cos_theta_0 = std::cos(theta_0);
        float theta = std::atan(std::tan(theta_0) - current_point.first / (velocity * velocity * cos_theta_0 * cos_theta_0));
        if (cos_theta_0 < 0)
        {
            theta -= M_PI;
        }
        return bound_angle(theta);
    }
}

AngleRange calc_current_angles(AngleRange angles, float velocity, std::pair<Coords, Coords> calculation_points)
{
    AngleRange result;

    result.first = current_angle(angles.first, velocity, calculation_points.first);
    result.second = current_angle(angles.second, velocity, calculation_points.second);
    if (!correct_angle_order(result))
    {
        swap_pair(&result);
    }
    return result;
}

float projectile_trajectory(float x, float theta_0, float v_0)
{
    float cosine = std::cos(theta_0);
    if (isclose(cosine, 0, 1e-7))
    {
        throw std::invalid_argument("Projectile trajectory not defined with x when angle is vertical");
    }
    return std::tan(theta_0) * x - (x * x) / (2 * v_0 * v_0 * cosine * cosine);
}

float calc_start_angle(Coords point, float angle, float velocity)
{
    //TODO: Not quite sure what of all the extra stuff is needed, for now just use a simple approach

    if (isclose(point.first, 0, 1e-7))
    {
        return angle;
    }

    float thetas[2];

    float velocity_2 = velocity * velocity;
    float velocity_4 = velocity_2 * velocity_2;
    float under_root = velocity_4 - 4 * point.first * (point.first + velocity_2 * std::tan(angle));
    if (under_root < 0)
    {
        throw std::invalid_argument("Angle not achievable");
    }
    float root = std::sqrt(under_root);

    thetas[0] = std::atan((velocity_2 + root) / (2 * point.first));
    thetas[1] = std::atan((velocity_2 - root) / (2 * point.first));

    //TODO: not sure if this is enough for testing if the angles work
    if (isclose(projectile_trajectory(point.first, thetas[0], velocity), point.second, 0.5))
    {
        return thetas[0];
    }
    else if (isclose(projectile_trajectory(point.first, thetas[1], velocity), point.second, 0.5))
    {
        return thetas[1];
    }
    throw std::invalid_argument("Point " + std::to_string(point.first) + "," + std::to_string(point.second) + " not reachable at angle " + std::to_string(angle));
}

float vector_to_angle(vec<float, 2> v)
{
    return std::atan2(A1(v), A0(v));
}

float vector_angle_between(vec<float, 2> v1, vec<float, 2> v2)
{
    auto a1 = vector_to_angle(v1);
    auto a2 = vector_to_angle(v2);
    float result = angle_diff(a1, a2, true, true, true);
    return result;
}

float time_point(float x, float velocity, float theta_0)
{
    return x / (velocity * std::cos(theta_0));
}

vec<float, 2> current_velocity(float x, float velocity, float theta_0)
{
    float v_y = velocity * std::sin(theta_0) - time_point(x, velocity, theta_0);
    float v_x = velocity * cos(theta_0);
    return {v_x, v_y};
}

std::pair<float, float> calc_bounce(Coords edge_point, std::pair<float, float> start_angle_and_current_angle, float velocity, vec<float, 2> wall_normal_vector)
{
    // Bounce variance?

    auto v_current = current_velocity(edge_point.first, velocity, start_angle_and_current_angle.first);
    // std::cout << "v_current " << v_current.a[0] << "," << v_current.a[1] << std::endl;

    if (vector_angle_between(v_current, wall_normal_vector) < M_PI_2)
    {
        // std::cout << "Cannot bounce inside a wall, so adjusting the angle that it should work" << std::endl;
        vec<float, 2> rotated = {A1(wall_normal_vector), -A0(wall_normal_vector)};
        v_current = vector_angle_between(v_current, rotated) < M_PI_2 ? rotated : -rotated;
        //FIXME this might be problematic at some point, but should not make problems
        // throw std::invalid_argument("Cannot bounce inside a wall");
    }

    auto u = dot(v_current, wall_normal_vector) * wall_normal_vector;
    auto w = v_current - u;
    // std::cout << "u " << u.a[0] << "," << u.a[1] << std::endl;
    // std::cout << "w " << w.a[0] << "," << w.a[1] << std::endl;

    auto v_new = FRICTION * w - RESTITUTION * u;
    // std::cout << "v_new " << v_new.a[0] << "," << v_new.a[1] << std::endl;

    std::pair<float, float> result;
    result.first = vector_angle_between({1, 0}, v_new);
    result.second = mag(v_new);
    // std::cout << "result angle: " << result.first << " velocity: " << result.second << std::endl;
    return result;
}

std::pair<AngleRange, float> calc_bounce(Coords edge_point, std::tuple<AngleRange, AngleRange> start_range_and_current_angles, float velocity, vec<float, 2> wall_normal_vector)
{
    std::pair<AngleRange, float> result;

    auto first = calc_bounce(edge_point, Coords(std::get<0>(start_range_and_current_angles).first, std::get<1>(start_range_and_current_angles).first), velocity, wall_normal_vector);
    auto second = calc_bounce(edge_point, Coords(std::get<0>(start_range_and_current_angles).second, std::get<1>(start_range_and_current_angles).second), velocity, wall_normal_vector);
    float new_velocity = std::max(first.second, second.second);
    result.first = AngleRange(first.first, second.first);
    if (!correct_angle_order(result.first))
    {
        swap_pair(&result.first);
    }
    result.second = new_velocity;
    return result;
}
