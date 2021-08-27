#define PROLOG_MODULE "behind_the_corner"
#include "SWI-Prolog.h"
#include "SWI-Stream.h"
#include "SWI-cpp.h"
#include <iostream>
#include <cmath>
#include "scene.h"
#include "simulation.h"
using namespace std;

static PlAtom ATOM_start("start");
static PlAtom ATOM_target("target");
static PlAtom ATOM_target_type("target_type");
static PlAtom ATOM_target_size("target_size");
static PlAtom ATOM_angle_range("angle_range");
static PlAtom ATOM_velocity("velocity");
static PlAtom ATOM_gravity("gravity");
static PlAtom ATOM_edge_length("edge_length");
static PlAtom ATOM_maxiter("maxiter");

/**
 * loads a scene from memory by the submission of a pointer
 */
static Scene *
get_value_ex(PlTerm handle)
{
  long l = (long)handle;

  Scene *t = (Scene *)(intptr_t)l;

  if (t->magic == SCENE_VALUE_MAGIC)
  {
    return t;
  }

  throw PlExistenceError(handle, "scene");
}

// PREDICATE(add_to_scene, 2)
// {
//   return
// }

/**
 * load_scene(-file,+handle)
 */
PREDICATE(load_scene, 2)
{
  const char *filename = A1;
  Scene *scene = new Scene(filename);
  if (!scene->load())
  {
    return FALSE;
  }
  A2 = (intptr_t)scene;
  return TRUE;
}

/**
 * connect
 * 
 * Predicate to validate if connection to this library works
 */
PREDICATE(connect, 0)
{
  return TRUE;
}

PREDICATE(get_scene_value, 5)
{
  Scene *scene = get_value_ex(A1);
  int x = (int)A2, y = (int)A3;

  const char *field_value = A4;
  if (A4 == "normal")
  {
    PlTail normal(A5);
    Zone zone = scene->getZone(x, y);
    normal.append(zone.getX());
    normal.append(zone.getY());
    normal.close();
  }
  else if (A4 == "ab_type")
  {
    A5 = scene->getZone(x, y).ab_type;
  }
  else
  {
    throw PlTypeError("normal|ab_type", A4);
  }
  return TRUE;
}

PREDICATE(free_scene, 1)
{
  Scene *scene = get_value_ex(A1);
  // cout << scene.file << endl;
  scene->magic = 0;
  delete scene;
  return TRUE;
}
std::pair<float, float> parse_coords_pair(PlTerm term, int edge_length, int height)
{
  PlTail list(term);
  if (list.arity() != 2)
    throw PlTypeError("list[2]", term);
  PlTerm first_coord;
  PlTerm second_coord;
  list.next(first_coord);
  list.next(second_coord);

  double first_coord_double = first_coord;
  double second_coord_double = second_coord;

  return Coords((int)(first_coord_double / edge_length),(int) (height - second_coord_double ) / edge_length);
}
std::pair<float, float> parse_float_pair(PlTerm term, int edge_length)
{
  PlTail list(term);
  if (list.arity() != 2)
    throw PlTypeError("list[2]", term);
  PlTerm first_coord;
  PlTerm second_coord;
  list.next(first_coord);
  list.next(second_coord);

  return Coords((double)first_coord / edge_length, (double)second_coord / edge_length);
}
std::pair<float, float> parse_float_pair(PlTerm term)
{
  return parse_float_pair(term, 1);
}

PREDICATE(simulate, 3)
{
  Scene *scene = get_value_ex(A1);

  if (A2.type() != PL_DICT)
    throw PlTypeError("dict", A2);
  PlTerm start_term,
      target_term,
      target_type_term,
      target_size_term,
      angle_range_term,
      velocity_term,
      gravity_term,
      edge_length_term,
      maxiter_term;

  PL_get_dict_key(ATOM_start.handle, A2, start_term);
  PL_get_dict_key(ATOM_target.handle, A2, target_term);
  PL_get_dict_key(ATOM_target_type.handle, A2, target_type_term);
  PL_get_dict_key(ATOM_target_size.handle, A2, target_size_term);
  PL_get_dict_key(ATOM_angle_range.handle, A2, angle_range_term);
  PL_get_dict_key(ATOM_velocity.handle, A2, velocity_term);
  PL_get_dict_key(ATOM_gravity.handle, A2, gravity_term);
  PL_get_dict_key(ATOM_edge_length.handle, A2, edge_length_term);
  PL_get_dict_key(ATOM_maxiter.handle, A2, maxiter_term);

  int edge_length = (int)edge_length_term;
  Coords start_coords = parse_coords_pair(start_term, edge_length, scene->height);
  Coords target_coords = parse_coords_pair(target_term, edge_length, scene->height);
  ABType target_type = ABType((int)target_type_term);
  float target_size = (double)target_size_term / edge_length;
  AngleRange angle_range = parse_float_pair(angle_range_term);
  float gravity = (double)gravity_term;
  // gravity is set to 1 for the whole simulation, and velocity needs to be respective to edge_length
  float velocity = (double)((double)velocity_term / (sqrt((double)gravity / edge_length) * edge_length));
  int maxiter = (int)maxiter_term;

  // cout << "Velocity before: " << (double)velocity_term << " after: " << velocity << endl;

  Simulation sim(start_coords, Target(target_coords, target_type, target_size), angle_range, velocity, maxiter);
  // Reduce scene size
  Scene current_scene;
  current_scene.loadFromScene(*scene, edge_length);
  try
  {
    /* code */
    auto results = sim.start(current_scene);
    PlTail pl_result(A3);
    for (auto result : results)
    {
      PlCompound one_result("btc_result", PlTermv(
                                              bound_angle_with_negative(result.first.first),
                                              bound_angle_with_negative(result.first.second),
                                              (result.second.first*edge_length + 0.5*edge_length),
                                              scene->height - (result.second.second*edge_length + 0.5*edge_length),
                                              0.0));
      pl_result.append(one_result);
    }
    pl_result.close();
    return TRUE;
  }
  catch (const NoPathFound &e)
  {
    PlTail pl_result(A3);
    for (auto result : e.closest_points)
    {
      PlCompound one_result("btc_close", PlTermv(
                                              bound_angle_with_negative(get<0>(result).first),
                                              bound_angle_with_negative(get<0>(result).second),
                                              (get<1>(result).first*edge_length),
                                              scene->height - (get<1>(result).second*edge_length),
                                              get<2>(result)
                                              ));
      pl_result.append(one_result);
    }
    pl_result.close();
    return TRUE;
  }
  catch (const TargetNotReachable &e)
  {
    return FALSE;
  }
  catch (const std::exception &e)
  {
    std::cerr << e.what() << '\n';
  }
  catch (const char *c)
  {
    std::cerr << c << "\n";
  }
  return FALSE;
}