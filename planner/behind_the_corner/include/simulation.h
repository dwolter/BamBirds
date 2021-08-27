#ifndef _BEHIND_THE_CORNER_SIMULATION_H_INCLUDED
#define _BEHIND_THE_CORNER_SIMULATION_H_INCLUDED
#include <tuple>
#include <vector>
#include <tuple>
#include "abType.h"
#include "scene.h"
#include "utils.h"
#include <boost/qvm/vec.hpp>

#define MAX_DISTANCE_TO_TARGET 

template<typename T>
using prioritized_item = std::pair<float,T>;

class Target {
public:
    Coords coords;
    ABType type;
    float size;
    Target(Coords coords, ABType type, float size) : coords(coords), type(type), size(size) {}
};

class State {
private:
    bool log_enabled = false;
public:
    State* turn;
    State* prev;
    Coords coords;
    ZoneEdge entry_edge;
    MovementType mt;
    AngleRange theta_range;
    double velocity;
    // If the current state is in a high or low angle -> reduces amount of calculations
    State();
    State(
        State* turn,
        State* prev,
        Coords coords,
        ZoneEdge entry_edge,
        MovementType mt,
        AngleRange theta_range,
        double velocity
    ) : 
        turn(turn), 
        prev(prev), 
        coords(coords), 
        entry_edge(entry_edge),
        mt(mt),
        theta_range(theta_range),
        velocity(velocity)
        {}
    std::vector<State*> next_states(const Scene &scene, Target target);
    AngleRange getShotAngleRange();
    Coords getFirstTurnPoint();
    State* getFirstTurnState();
private:
    std::vector<State*> next_states_for_exit_edge(const Scene &scene, Target target, ZoneEdge exit_edge);
    std::vector<std::tuple<AngleRange, AngleRange>> apply_gravity(Coords new_coords, ZoneEdge exit_edge);
    std::tuple<AngleRange, double, ZoneEdge> bounce(std::tuple<AngleRange, AngleRange> initial_angle_range, ZoneEdge exit_edge, boost::qvm::vec<float, 2> normal_vector);
};

class SimulationException : public std::exception {};

class TargetNotReachable : public SimulationException 
{};

class NoPathFound : public SimulationException 
{
public:
    std::vector<std::tuple<AngleRange, Coords, float>> closest_points;
    NoPathFound(std::vector<std::tuple<AngleRange, Coords, float>> closest_points) : closest_points(closest_points) {}
};

class Simulation {
public: 
    Coords start_coords;
    Target target;
    AngleRange start_range;
    double velocity;
    double maxiter;
    
    Simulation(
        Coords start_coords,
        Target target,
        AngleRange start_range,
        double velocity,
        double maxiter
    ) :
        start_coords(start_coords),
        target(target),
        start_range(start_range),
        velocity(velocity),
        maxiter(maxiter) {}
    /**
     * @return tuples with angle ranges and their target points
     */
    std::vector<std::pair<AngleRange, Coords>> start(Scene &scene);
};

#endif