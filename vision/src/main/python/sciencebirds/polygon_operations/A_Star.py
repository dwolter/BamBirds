from . import Polygon as p
from . import SplitPolygon as sp
import math


def is_goal(polygons_list: list) -> bool:
    for polygon in polygons_list:
        if not polygon.is_primitive():
            return False
    return True


def calc_heuristic(polygons_list: list) -> int:
    heuristic = 0
    for polygon in polygons_list:
        if not polygon.is_primitive():
            heuristic = heuristic + 1
    return heuristic


def split_A_star(polygon, debug=False):
    # polygon is our start-state
    # split_polygon performs an expand-state operation
    cost_start = 0
    if polygon.is_primitive():
        heuristic_start = 0
    else:
        heuristic_start = 1

    polygon_states = [
        ([polygon], (cost_start, heuristic_start), heuristic_start + cost_start)]
    while len(polygon_states) > 0:
        current_state = polygon_states.pop(0)
        if is_goal(current_state[0]):
            return current_state[0]
        # if len(polygon_states) > 10000:
            # return current_state[0]

        for p in current_state[0]:
            if not p.is_primitive():
                prev_polygons = current_state[0].copy()
                prev_polygons.remove(p)

                p_expanded_states = sp.split_polygon(p)
                for state in p_expanded_states:
                    new_polygons = prev_polygons + state[0]
                    new_cost = current_state[1][0] + state[1][0]
                    new_heuristic = state[1][1]
                    new_state = (
                        new_polygons, (new_cost, new_heuristic), new_cost + new_heuristic)
                    polygon_states.append(new_state)

        polygon_states = sorted(
            polygon_states, key=lambda x: x[2], reverse=False)
        # print("hi")

        # n = math.floor(len(polygon_states)*3/4)
        if debug:
            print("states: " + str(len(polygon_states)))
        # for i in range(0, n):
        # polygon_states.pop()


if __name__ == "__main__":
    test_vertices = [[38.7, 18.3], [43.9, 18.3], [43.9, 18.6], [43.5, 18.6], [43.3, 18.8], [43.3, 19.2], [43.0, 19.2],
                     [43.0, 18.9], [43.1, 18.8], [42.9, 18.6], [42.4, 18.6], [
                         42.2, 18.8], [42.2, 19.2], [42.1, 19.3],
                     [42.1, 19.4], [42.3, 19.6], [43.3, 19.6], [43.3, 19.9], [
                         42.6, 19.9], [42.4, 20.1], [42.5, 20.2],
                     [42.5, 20.5], [42.4, 20.4], [41.6, 20.4], [41.6, 20.1], [
                         41.4, 19.9], [40.9, 19.9], [39.8, 20.2],
                     [39.9, 20.1], [39.7, 19.9], [38.9, 19.9], [38.9, 19.6], [
                         39.0, 19.5], [38.8, 19.3], [38.6, 19.3],
                     [38.6, 19.0], [39.4, 19.0], [39.5, 18.9], [39.5, 19.4], [
                         39.7, 19.6], [40.0, 19.6], [40.2, 19.4],
                     [40.1, 19.3], [40.1, 18.9], [40.2, 18.8], [40.0, 18.6]]

    test_poly = p.Polygon(test_vertices)
    result = split_A_star(test_poly)
    print(result)
    for r in result:
        print(r.vertices)
