from . import Polygon as p
import math

# Parameters
# for function cut_polygon()
COST_STRAIGHT = 1
COST_STEEP = 1
SLOPE_MARGIN = 0.5

# for function split_polygon()
CLOSEST_NUM = 3


def calc_heuristic(polygon, primitive):
    # number of lines in polygon
    N = len(polygon.vertices)
    n = len(primitive.vertices)

    heuristic = max(2, math.ceil(N / (n - 1)))

    return heuristic


def get_closest(vertex, vertices, n):
    dists = []
    for i in range(len(vertices)):
        if vertices[i][0] == -1 and vertices[i][1] == -1:
            continue
        dist = p.calc_dist(vertex, vertices[i])
        dists.append((i, dist))

    closest = sorted(dists, key=lambda x: x[1], reverse=False)
    closest = closest[0:n]

    return closest


def cut_polygon(polygon, src_index, dst_index):
    # the source and dst here are represented by their indexes not actual values
    vertices = polygon.vertices
    vertices_len = len(vertices)
    polygon1_vertices = []
    polygon2_vertices = []

    i = src_index
    while i != dst_index:
        polygon1_vertices.append(vertices[i])
        i = i + 1
        if i == vertices_len:
            i = 0
    polygon1_vertices.append(vertices[dst_index])

    j = dst_index
    while j != src_index:
        polygon2_vertices.append(vertices[j])
        j = j + 1
        if j == vertices_len:
            j = 0
    polygon2_vertices.append(vertices[src_index])

    polygon1 = p.Polygon(polygon1_vertices)
    polygon2 = p.Polygon(polygon2_vertices)

    src_vertex = vertices[src_index]
    dst_vertex = vertices[dst_index]
    slope = abs(p.calc_slope(src_vertex, dst_vertex))

    if slope <= SLOPE_MARGIN:
        cost = COST_STRAIGHT
    else:
        cost = COST_STEEP

    # calculate costs and heuristics
    if polygon1.is_primitive():
        return [polygon1, polygon2], cost

    elif polygon2.is_primitive():
        return [polygon2, polygon1], cost

    else:
        return [-1, -1], cost


def split_polygon(polygon):
    vertices = polygon.vertices
    vertices_len = len(vertices)
    cuts = []

    for i, v in enumerate(vertices, 0):
        src_vertex_index = i
        if i + 2 == vertices_len - 1:
            dst_vertex_index = i + 2
            line = [polygon.vertices[i], polygon.vertices[dst_vertex_index]]
            if polygon.intersects(line, False) == 0 and polygon.in_lies(line):
                polygon_cut = cut_polygon(
                    polygon, src_vertex_index, dst_vertex_index)
                if not polygon_cut[0][0] == -1:
                    polygon_primitive = polygon_cut[0][0]
                    rest_polygon = polygon_cut[0][1]
                    cost = polygon_cut[1]
                    heuristic = calc_heuristic(polygon, polygon_primitive) - 1
                    final_cut = [
                        polygon_primitive, rest_polygon], (cost, heuristic), (cost + heuristic)
                    cuts.append(final_cut)

        elif i + 2 > vertices_len - 1:
            break

        else:

            if i == 0:
                rest_vertices = vertices[i + 2: vertices_len - 1]
            else:
                rest_vertices = vertices[i + 2: vertices_len]

            for j in range(len(rest_vertices)):
                dst_vertex_index = j + i + 2
                line = [polygon.vertices[i],
                        polygon.vertices[dst_vertex_index]]
                if polygon.intersects(line, False) != 0 or not polygon.in_lies(line):
                    rest_vertices[j] = [-1, -1]

            closest_vertices = get_closest(v, rest_vertices, CLOSEST_NUM)
            for close_vertex in closest_vertices:
                dst_vertex_index = close_vertex[0] + i + 2
                polygon_cut = cut_polygon(
                    polygon, src_vertex_index, dst_vertex_index)

                if not polygon_cut[0][0] == -1:
                    polygon_primitive = polygon_cut[0][0]
                    rest_polygon = polygon_cut[0][1]
                    cost = polygon_cut[1]
                    heuristic = calc_heuristic(polygon, polygon_primitive) - 1
                    final_cut = [
                        polygon_primitive, rest_polygon], (cost, heuristic), (cost + heuristic)
                    cuts.append(final_cut)

    return cuts


if __name__ == "__main__":
    test_vertices = [[40.0, 19.6], [40.2, 19.4], [40.1, 19.3], [40.1, 18.9], [40.2, 18.8], [40.0, 18.6], [39.7, 18.6], [39.6, 18.7], [39.5, 18.7], [39.4, 18.6], [38.7, 18.3], [43.9, 18.3], [43.9, 18.6], [43.5, 18.6], [43.3, 18.8], [43.3, 19.2], [43.0, 19.2], [43.0, 18.9], [43.1, 18.8], [
        42.9, 18.6], [42.4, 18.6], [42.2, 18.8], [42.2, 19.2], [42.1, 19.3], [42.1, 19.4], [42.3, 19.6], [43.3, 19.6], [43.3, 19.9], [42.6, 19.9], [42.4, 20.1], [42.5, 20.2], [42.5, 20.5], [42.4, 20.4], [41.6, 20.4], [41.6, 20.1], [41.4, 19.9], [40.9, 19.9], [40.7, 20.1], [40.7, 20.7], [39.9, 20.8]]

    test_poly = p.Polygon(test_vertices)
    # print(test_poly.refine().vertices)
    cutss = split_polygon(test_poly)
    # cutss2 = split_polygon(test_poly.refine())

    # cutss = sorted(cutss, key=lambda x: x[2], reverse=False)

    # print(len(cutss))
    # print(len(cutss2))

    ii = 0
    for c in cutss:
        print('cut : ' + str(ii))
        print("poly 1: " + str(c[0][0].vertices))
        print("poly 2: " + str(c[0][1].vertices))
        print("heuristik: " + str(c[1]))

        ii = ii + 1
