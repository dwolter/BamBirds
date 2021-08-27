#include <iostream>
#include <cmath>
#include <string>
#include <vector>
#include <queue>
#include <tuple>
#include <unordered_set>
#include <map>
#include <set>
#include <stdexcept>
#include <chrono>
#include <algorithm>
// #include <CImg.h>

#include "abType.h"
#include "simulation.h"
#include "utils.h"
#include "scene.h"
#include "fixed_size_priority_queue.h"

using namespace std;
// using namespace cimg_library;

template <typename T>
struct PrioritizedCompare
{
    bool operator()(const prioritized_item<T> &lhs, const prioritized_item<T> &rhs)
    {
        // Return other way around since we want lowest first
        return lhs.first > rhs.first;
    }
};

bool free_zone(const Scene &scene, Coords coords)
{
    return scene.getZone(coords).ab_type == Background;
}

bool hill_between(const Scene &scene, Coords a, Coords b)
{
    for (Coords point : bresenham(a, b))
    {
        if (scene.getZone(point).ab_type == Hill)
        {
            return true;
        }
    }
    return false;
}

bool hits_target(const Scene &scene, Target target, Coords current)
{
    return euclidean_2d_dist(target.coords, current) < target.size * 1.5 &&
           scene.getZone(current).ab_type == target.type &&
           !hill_between(scene, current, target.coords);
}

vector<pair<Coords, float>> coord_neighbours_with_distance(Coords point, bool eight_point)
{
    vector<pair<Coords, float>> result;
    int x = get<0>(point);
    int y = get<1>(point);
    // TODO: Maybe make these distances reflect edge_length?
    result.push_back(pair<Coords, float>(Coords(x, y + 1), 1));
    result.push_back(pair<Coords, float>(Coords(x + 1, y), 1));
    result.push_back(pair<Coords, float>(Coords(x, y - 1), 1));
    result.push_back(pair<Coords, float>(Coords(x - 1, y), 1));
    if (eight_point)
    {
        result.push_back(pair<Coords, float>(Coords(x + 1, y + 1), M_SQRT2));
        result.push_back(pair<Coords, float>(Coords(x - 1, y - 1), M_SQRT2));
        result.push_back(pair<Coords, float>(Coords(x + 1, y - 1), M_SQRT2));
        result.push_back(pair<Coords, float>(Coords(x - 1, y + 1), M_SQRT2));
    }
    return result;
}

const float getDistanceValue(const vector<vector<float>> &distance_matrix, const Coords &c)
{
    int xInt = c.first, yInt = c.second;
    int x = min((int)distance_matrix.size() - 1, max(xInt, 0));
    int y = min((int)distance_matrix[0].size() - 1, max(yInt, 0));
    return distance_matrix.at(x).at(y) + euclidean_2d_dist({xInt, yInt}, {x, y});
}

vector<pair<AngleRange, Coords>> Simulation::start(Scene &scene)
{

    auto start = chrono::steady_clock::now();
    vector<vector<float>> dijkstra_distance(scene.width, vector<float>(scene.height, INFINITY));
    priority_queue<prioritized_item<Coords>, vector<prioritized_item<Coords>>, PrioritizedCompare<Coords>> dijkstra_queue;

    prioritized_item<Coords> dijkstra_initial_coords(0, target.coords);
    dijkstra_queue.push(dijkstra_initial_coords);
    dijkstra_distance[target.coords.first][target.coords.second] = 0;
    float max_distance = 0;

    while (!dijkstra_queue.empty())
    {
        Coords cur = get<1>(dijkstra_queue.top());
        dijkstra_queue.pop();
        for (pair<Coords, float> neighbour_with_distance : coord_neighbours_with_distance(cur, true))
        {
            Coords neighbour = neighbour_with_distance.first;
            float prev_dist = getDistanceValue(dijkstra_distance, cur);
            float distance = prev_dist + neighbour_with_distance.second;
            if (inside_bounds(neighbour, scene.width, scene.height) &&
                (scene.getZone(neighbour).ab_type == Background || scene.getZone(neighbour).ab_type == target.type) &&
                distance < getDistanceValue(dijkstra_distance, neighbour))
            {
                dijkstra_distance[neighbour.first][neighbour.second] = distance;
                dijkstra_queue.push(prioritized_item<Coords>(distance, neighbour));
                if (distance > max_distance)
                    max_distance = distance;
            }
        }
    }

    auto end = chrono::steady_clock::now();
    auto diff = end - start;

    // cout << "Dijkstra done in " << chrono::duration <double, milli> (diff).count() << " ms" << endl;
    // cout << "Distance of start: "<< dijkstra_distance[start_coords.first][start_coords.second] << endl;
    // cout << "Highest distance: " << max_distance << endl;

    if (getDistanceValue(dijkstra_distance, start_coords) == INFINITY)
    {
        throw TargetNotReachable();
    }

    // CImg dijkstra(scene.width, scene.height);
    // for (int x = 0; x < scene.width; x++)
    // {
    //     for (int y = 0; y < scene.height; y++)
    //     {
    //         dijkstra.atXY(x,scene.height - y - 1) = dijkstra_distance[x][y] == INFINITY ? max_distance : dijkstra_distance[x][y];
            
    //     }
    // }
    // CImg normalX(scene.width, scene.height);
    // for (int x = 0; x < scene.width; x++)
    // {
    //     for (int y = 0; y < scene.height; y++)
    //     {
    //         normalX.atXY(x,scene.height - y - 1) = scene.getZone(x,y).normal.a[0];

    //     }
    // }
    // CImg normalY(scene.width, scene.height);
    // for (int x = 0; x < scene.width; x++)
    // {
    //     for (int y = 0; y < scene.height; y++)
    //     {
    //         normalY.atXY(x,scene.height - y - 1) = scene.getZone(x,y).normal.a[1];

    //     }
    // }
    // CImgDisplay display(dijkstra);
    // display.show();
    // while(!display.is_closed())
    // {
    //     sleep(1);
    // }
    // display.display(normalX);
    // display.show();
    // while(!display.is_closed())
    // {
    //     sleep(1);
    // }
    // display.display(normalY);
    // display.show();
    // while(!display.is_closed())
    // {
    //     sleep(1);
    // }

    start = chrono::steady_clock::now();
    priority_queue<prioritized_item<State *>, vector<prioritized_item<State *>>, PrioritizedCompare<State *>> best_first_queue;
    fixed_size_priority_queue<prioritized_item<State *>, PrioritizedCompare<State *>> closest_states(10);

    unordered_set<int> visited_target_points;
    map<Coords, int> visited_points;

    int iteration = 0;
    State initial_state(nullptr, nullptr, start_coords, LEFT, FLY, start_range, velocity);
    best_first_queue.push(prioritized_item<State *>(getDistanceValue(dijkstra_distance, start_coords), &initial_state));
    closest_states.push(prioritized_item<State *>(getDistanceValue(dijkstra_distance, start_coords), &initial_state));
    vector<pair<AngleRange, Coords>> results;
    while (!best_first_queue.empty() && iteration < maxiter)
    {
        auto top = best_first_queue.top();
        best_first_queue.pop();

        if (!visited_points.emplace(top.second->coords, 1).second)
        {
            visited_points.at(top.second->coords)++;
        }
        // cout << "with mem: " << top.second << " with prev: " << top.second->prev << " with turn: " << top.second->turn << endl;
        // check if state with same first turn point has been removed
        auto first_turn = top.second->getFirstTurnPoint();
        int target_point_value = first_turn.first + first_turn.second * scene.width;
        // .contains only since c++20
        if (visited_target_points.find(target_point_value) != visited_target_points.end())
        {
            continue;
        }
        iteration++;
        auto v = top.second;
        auto new_states = v->next_states(scene, target);
        for (auto s : new_states)
        {
            // cout << "New State: " << s->coords.first << "," << s->coords.second << endl;
            // cout << " with angles: " << s->theta_range.first << "," << s->theta_range.second << endl;
            // cout << " with entryedge: " << s->entry_edge << endl;
            // cout << " with movementtype: " << s->mt << endl;
            if (s->mt == GOAL)
            {
                // add states first turnpoint to removed states
                auto s_first_turn = s->getFirstTurnPoint();
                int s_target_point_value = first_turn.first + first_turn.second * scene.width;
                visited_target_points.insert(s_target_point_value);

                results.push_back(pair<AngleRange, Coords>(s->getShotAngleRange(), s->getFirstTurnPoint()));
                continue;
            }
            prioritized_item<State *> ps(getDistanceValue(dijkstra_distance, s->coords), s);
            best_first_queue.push(ps);
            closest_states.push(ps);
        }
        // cout << " Done with state expansion" << endl;
    }
    end = chrono::steady_clock::now();
    diff = end - start;
    // cout << "Simulation done in " << chrono::duration <double, milli> (diff).count() << " ms" << " because " << (iteration >= maxiter ? "iteration limit hit" : "no states found after " + to_string(iteration) + " iterations") << endl;
    // CImg<float> visitedIMG(scene.width, scene.height);
    // for (auto visited_point : visited_points)
    // {
    //     cout << visited_point.first.first << "," << visited_point.first.second << ": " << visited_point.second << endl;
    //     visitedIMG.atXY(visited_point.first.first,scene.height - visited_point.first.second - 1) = visited_point.second;
    // }
    // CImgDisplay display(visitedIMG);
    // display.show();
    // while(!display.is_closed())
    // {
    //     sleep(1);
    // }

    if (results.size() == 0)
    {
        std::vector<std::tuple<AngleRange, Coords, float>> close_states_result;
        while (!closest_states.empty())
        {

            State *s = closest_states.top().second;
            float dist = closest_states.top().first;
            float dist_ratio = abs(dist / getDistanceValue(dijkstra_distance, start_coords));
            // cout << "Closest State: dist: " << closest_states.top().first << " coords: " << s->coords.first << "," << s->coords.second;
            // cout << " angles: " << s->theta_range.first << "," << s->theta_range.second;
            // cout << " entry: " << s->entry_edge;
            // cout << " movementtype: " << s->mt << endl;
            closest_states.pop();
            if (s != s->getFirstTurnState())
            {
                close_states_result.push_back(tuple<AngleRange, Coords, float>(s->getShotAngleRange(), s->getFirstTurnPoint(), dist_ratio));
            }
            else
            {
                close_states_result.push_back(tuple<AngleRange, Coords, float>(s->getShotAngleRange(), s->coords, dist_ratio));
            }
        }
        throw NoPathFound(close_states_result);
    }
    // for (auto res : results)
    // {
    //     cout << "[" << res.first.first << "," << res.first.second << "] (" << res.second.first << "|" << res.second.second << ")" << endl;
    // }
    return merge_intervals(results);
}

vector<State *> State::next_states(const Scene &scene, Target target)
{
    vector<State *> result, cw_res, ccw_res, par_res;
    cw_res = next_states_for_exit_edge(scene, target, cw(entry_edge));
    ccw_res = next_states_for_exit_edge(scene, target, ccw(entry_edge));
    par_res = next_states_for_exit_edge(scene, target, par(entry_edge));
    result.insert(result.end(), cw_res.begin(), cw_res.end());
    result.insert(result.end(), ccw_res.begin(), ccw_res.end());
    result.insert(result.end(), par_res.begin(), par_res.end());
    // if(result.size() == 0)
    // {
    //     cout << endl << "Failed State: coords: " << coords.first << "," << coords.second;
    //     cout << " angles: " << theta_range.first << "," << theta_range.second;
    //     cout << " velocity: " << velocity;
    //     cout << " entry: " << entry_edge;
    //     cout << " movementtype: " << mt << endl;
    //     log_enabled = true;
    //     cout << " cw: " << endl;
    //     next_states_for_exit_edge(scene, target, cw(entry_edge));
    //     cout << " ccw: " << endl;
    //     next_states_for_exit_edge(scene, target, ccw(entry_edge));
    //     cout << " par: " << endl;
    //     next_states_for_exit_edge(scene, target, par(entry_edge));
    // }
    return result;
}

bool is_looping(State *state, int number_of_loops)
{
    State *s = state->prev;
    int i = 0;
    int count_looping;
    while (s != nullptr && i < number_of_loops)
    {
        if (s->coords == state->coords)
        {
            count_looping++;
        }
        i++;
        s = s->prev;
    }
    return count_looping == number_of_loops;
}

vector<State *> State::next_states_for_exit_edge(const Scene &scene, Target target, ZoneEdge exit_edge)
{
    vector<State *> result;

    if (is_looping(this, 2))
    {
        return result;
    }

    Coords next_zone = neighbour(coords, exit_edge);

    State *new_turn = turn == nullptr ? this : turn;
    if (inside_bounds(next_zone, scene.width, scene.height, true))
    {
        if (hits_target(scene, target, next_zone))
        {
            result.push_back(new State(new_turn, this, next_zone, par(exit_edge), GOAL, theta_range, velocity));
        }
        auto new_ranges_with_current_angles = apply_gravity(next_zone, exit_edge);
        for (auto new_range_with_velocity_with_current_angles : new_ranges_with_current_angles)
        {
            try
            {
                switch (mt)
                {
                case BOUNCE:
                    // TODO: sliding
                    new_turn = this;
                case FLY:
                    if (free_zone(scene, next_zone))
                    {
                        result.push_back(new State(new_turn, this, next_zone, par(exit_edge), FLY, get<0>(new_range_with_velocity_with_current_angles), velocity));
                    }
                    else
                    {
                        auto new_range_with_velocity_after_bounce = bounce(new_range_with_velocity_with_current_angles, exit_edge, scene.getZoneVector(next_zone));
                        if (get<1>(new_range_with_velocity_after_bounce) > 0.1)
                            result.push_back(new State(new_turn, this, this->coords, get<2>(new_range_with_velocity_after_bounce), BOUNCE, get<0>(new_range_with_velocity_after_bounce), get<1>(new_range_with_velocity_after_bounce)));
                    }
                    break;
                case SLIDE:
                    // TODO: sliding
                    break;
                default:
                    break;
                }
            }
            catch (const std::exception &e)
            {
                if (log_enabled)
                {
                    std::cout << e.what() << '\n';
                }
            }
        }
    }

    return result;
}

float first_decimal_place_difference(float a, float b)
{
    return fabs(a - b);
}

vector<tuple<AngleRange, AngleRange>> State::apply_gravity(Coords new_coords, ZoneEdge exit_edge)
{
    vector<tuple<AngleRange, AngleRange>> options;
    AngleRange possible_angle_range = possible_angles(entry_edge, exit_edge);

    ZoneEdge entry_edge_0 = entry_edge;

    Coords edge_point_0;
    if (mt == BOUNCE || turn == nullptr)
    {
        edge_point_0 = edge_coordinates(coords, entry_edge);
    }
    else
    {
        entry_edge_0 = turn->entry_edge;
        edge_point_0 = edge_coordinates(turn->coords, turn->entry_edge);
    }
    if (log_enabled)
        cout << "====" << endl
             << "edge_point_0 " << edge_point_0.first << "," << edge_point_0.second << endl;
    Coords edge_point_new = edge_coordinates(new_coords, par(exit_edge));
    if (log_enabled)
        cout << "edge_point_new " << edge_point_new.first << "," << edge_point_new.second << endl;
    if (log_enabled)
        cout << "entry: " << entry_edge << " exit: " << exit_edge << endl;
    // Calculation points (end and start points of the exit edge, relative to the current edge_point)
    pair<Coords, Coords> calculation_points = get_calculation_points(edge_point_0, edge_point_new, entry_edge, exit_edge);
    if (log_enabled)
        cout << "calculation points " << calculation_points.first.first << "," << calculation_points.first.second << " " << calculation_points.second.first << "," << calculation_points.second.second << endl;
    vector<IsHigh> is_high_options;
    is_high_options.push_back(IsHigh_True);
    is_high_options.push_back(IsHigh_False);

    if (log_enabled)
        cout << "starting_angles " << theta_range.first << "," << theta_range.second << endl;
    for (IsHigh is_high_calc : is_high_options)
    {
        try
        {
            if (log_enabled)
                cout << "is_high " << is_high_calc << endl;
            AngleRange angles = angles_to_hit_edge(velocity, calculation_points, is_high_calc);
            if (log_enabled)
                cout << "angles_to_hit " << angles.first << "," << angles.second << endl;

            angles = interval_intersection(angles, theta_range);
            if (log_enabled)
                cout << "intersected " << angles.first << "," << angles.second << endl;

            //TODO: On Parabola check necessary?

            AngleRange current_angles = calc_current_angles(angles, velocity, calculation_points);
            if (log_enabled)
                cout << "current_angles " << current_angles.first << "," << current_angles.second << endl;

            if (log_enabled)
                cout << "possible angles " << possible_angle_range.first << "," << possible_angle_range.second << endl;
            AngleRange intersected = interval_intersection(possible_angle_range, current_angles);
            if (log_enabled)
                cout << "intersected " << intersected.first << "," << intersected.second << endl;

            if (!isclose(intersected.first, current_angles.first, 1e-5))
            {
                float starting_angle = calc_start_angle(calculation_points.first, intersected.first, velocity);
                if (in_angle_interval(starting_angle, angles))
                {
                    angles.first = starting_angle;
                }
            }

            if (!isclose(intersected.second, current_angles.second, 1e-5))
            {
                float starting_angle = calc_start_angle(calculation_points.second, intersected.second, velocity);
                if (in_angle_interval(starting_angle, angles))
                {
                    angles.second = starting_angle;
                }
            }
            if (log_enabled)
                cout << "final " << angles.first << "," << angles.second << endl;
            options.push_back(tuple<AngleRange, AngleRange>(angles, intersected));
        }
        catch (const std::exception &e)
        {
            if (log_enabled)
                std::cout << e.what() << '\n';
        }
    }

    if (options.empty())
    {
        if (log_enabled && (mt == BOUNCE || (prev != nullptr && prev->mt == BOUNCE)))
        {
            cout << endl
                 << "Failed State coords: " << coords.first << "," << coords.second;
            cout << " velocity: " << velocity;
            cout << " angles: " << theta_range.first << "," << theta_range.second;
            cout << " difference: " << first_decimal_place_difference(theta_range.first, theta_range.second);
            cout << " entry: " << entry_edge;
            cout << " exit: " << exit_edge;
            cout << " movementtype: " << mt << endl;
            cout << "calculation points " << calculation_points.first.first << "," << calculation_points.first.second << " " << calculation_points.second.first << "," << calculation_points.second.second << endl;
        }
        return options;
    }
    vector<float> angles;
    for (auto option : options)
    {
        angles.push_back(get<0>(option).first);
        angles.push_back(get<0>(option).second);
    }
    AngleRange resulting_angle_range = to_angle_range(angles);
    AngleRange current_angles = calc_current_angles(resulting_angle_range, velocity, calculation_points);
    if (log_enabled && (mt == BOUNCE || (prev != nullptr && prev->mt == BOUNCE)))
    {
        cout << endl
             << "Worked bouncing State coords: " << coords.first << "," << coords.second;
        cout << " velocity: " << velocity;
        cout << " angles: " << theta_range.first << "," << theta_range.second;
        cout << " difference: " << first_decimal_place_difference(theta_range.first, theta_range.second);
        cout << " entry: " << entry_edge;
        cout << " exit: " << exit_edge;
        cout << " movementtype: " << mt << endl;
        cout << " new angles " << resulting_angle_range.first << "," << resulting_angle_range.second << endl;
        cout << "calculation points " << calculation_points.first.first << "," << calculation_points.first.second << " " << calculation_points.second.first << "," << calculation_points.second.second << endl;
    }
    options.clear();
    options.push_back(tuple<AngleRange, AngleRange>(resulting_angle_range, current_angles));
    return options;
}

std::tuple<AngleRange, double, ZoneEdge> State::bounce(std::tuple<AngleRange, AngleRange> angle_range_info, ZoneEdge exit_edge, boost::qvm::vec<float, 2> normal_vector)
{
    Coords current_edge = edge_coordinates(turn->coords, turn->entry_edge);
    // cout << "current_edge" << current_edge.first << "," << current_edge.second << endl;
    Coords new_edge = edge_coordinates(coords, exit_edge);
    // cout << "new_edge" << new_edge.first << "," << new_edge.second << endl;
    Coords relative_edge = relative_point(current_edge, new_edge);
    // cout << "relative_edge" << relative_edge.first << "," << relative_edge.second << endl;
    auto calc_res = calc_bounce(relative_edge, angle_range_info, velocity, normal_vector);
    ZoneEdge bounce_edge = exit_edge;
    // cout << endl << "Bouncing: " << coords.first << "," << coords.second;
    // cout << " angles before: " << theta_range.first << "," << theta_range.second;
    // cout << " angles now: " << calc_res.first.first << "," << calc_res.first.second;
    // cout << " vel before: " << velocity;
    // cout << " vel now: " << calc_res.second;
    // cout << " normal: " << normal_vector.a[0] << "," << normal_vector.a[1];
    // cout << " entry: " << entry_edge;
    // If the angle range intersects with the wall, the
    if (angle_intervals_intersect(calc_res.first, possible_angles(exit_edge, exit_edge), true))
    {
        ZoneEdge i = exit_edge;
        float max_interval_intersection_size = 0;
        do
        {
            if (angle_intervals_intersect(calc_res.first, possible_angles(i, par(i)), true))
            {
                float current_interval_intersection_size = angle_interval_size(interval_intersection(calc_res.first, possible_angles(i, par(i))));
                if (current_interval_intersection_size > max_interval_intersection_size)
                {
                    max_interval_intersection_size = current_interval_intersection_size;
                    bounce_edge = i;
                }
            }
            i = cw(i);
        } while (i != exit_edge);
    }

    // cout << " bounce: " << bounce_edge << endl;
    return {calc_res.first, calc_res.second, bounce_edge};
}

AngleRange State::getShotAngleRange()
{
    State *state = getFirstTurnState();
    if (state != nullptr)
        if (state->prev != nullptr)
            return state->prev->theta_range;
        else
            return theta_range;
    else
        return theta_range;
}
Coords State::getFirstTurnPoint()
{
    State *state = getFirstTurnState();
    if (state != nullptr)
        return state->coords;
    else
        return coords;
}
State *State::getFirstTurnState()
{
    State *cur_turn = turn;
    State *prev_turn = cur_turn;
    while (cur_turn != nullptr && cur_turn->turn != nullptr)
    {
        prev_turn = cur_turn;
        cur_turn = cur_turn->turn;
    }
    return prev_turn;
}
