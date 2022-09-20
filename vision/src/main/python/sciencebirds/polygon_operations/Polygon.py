import math
from . import Intersect as inter
import numpy as np

# Here you can set all parameters

# function refine()
REFINE_ANGLE_MARGIN = 50

# function is_rect()
RECT_DIAGONAL_MARGIN = 0.9

# function is_circle_2()

CIRCLE_DST_MARGIN = 0.8


def latex2poly(latex):
    poly = latex.replace('--', ',')
    poly = poly.replace('(', '[')
    poly = poly.replace(')', ']')
    return poly


def calc_angle(vertex1, vertex2):
    Double_PI = 6.2831853071795865
    if vertex1[1] == vertex2[1] and vertex1[0] == vertex2[0]:
        # print("It is the same point")
        return 0

    theta = math.atan2((vertex2[0] - vertex1[0]), (vertex2[1] - vertex1[1]))
    if theta < 0.0:
        theta += Double_PI
    return math.degrees(theta)


def calc_angle_to_y_axis(vertex1, vertex2, isUp):
    # angle relative to the x axis
    theta_x_rad = math.atan2(
        abs((vertex2[1] - vertex1[1])), abs((vertex2[0] - vertex1[0])))

    theta_x = math.degrees(theta_x_rad)
    # angle relative to the y axis
    if isUp:
        theta_y = 90 - theta_x
    else:
        theta_y = 90 + theta_x

    return theta_y


# calculate angle between lines
def calc_lines_angle(line1, line2):
    # a line is a list of two vertices
    m1 = calc_slope(line1[0], line1[1])
    m2 = calc_slope(line2[0], line2[1])
    # print(m1)
    # print(m2)
    if m1 == math.inf:
        if line1[0] in line2:
            return calc_angle_to_y_axis(line2[0], line2[1], True)
        else:
            return calc_angle_to_y_axis(line2[0], line2[1], False)

    elif m2 == math.inf:
        if line2[0] in line1:
            return calc_angle_to_y_axis(line1[0], line1[1], True)
        else:
            return calc_angle_to_y_axis(line1[0], line1[1], False)

    else:
        denominator = 1 + (m1 * m2)
        if denominator != 0:
            radian_angle = math.atan(((m1 - m2) / denominator))
            degrees_angle = math.degrees(radian_angle)
        else:
            degrees_angle = 90
    return degrees_angle


# calculate angle between lines
def calc_lines_angle2(line1, line2):
    # a line is a list of two vertices
    m1 = calc_slope(line1[0], line1[1])
    m2 = calc_slope(line2[0], line2[1])
    # print(m1)
    # print(m2)
    if m1 == math.inf:
        return math.degrees(math.atan((line2[1][1] - line2[0][1]) / (line2[1][0] - line2[0][0])))

    elif m2 == math.inf:
        return math.degrees(math.atan((line1[1][1] - line1[0][1]) / (line1[1][0] - line1[0][0])))

    else:
        denominator = 1 + (m1 * m2)
        if denominator != 0:
            radian_angle = math.atan(((m1 - m2) / denominator))
            degrees_angle = math.degrees(radian_angle)
        else:
            degrees_angle = 90
    return degrees_angle


def calc_dist(vertex1, vertex2):
    euclidean_dist = math.sqrt(math.pow(
        (vertex1[1] - vertex2[1]), 2) + math.pow((vertex1[0] - vertex2[0]), 2))
    return euclidean_dist


def calc_slope(vertex1, vertex2):
    # a vertex is a list of two elements, x and y respectively
    if vertex2[0] == vertex1[0]:
        return math.inf
    else:
        slope = ((vertex2[1] - vertex1[1]) / (vertex2[0] - vertex1[0]))
        return slope


class Polygon:
    def __init__(self, vertices):
        # vertices are set in anti-clock wise direction
        self.vertices = vertices

        # checks if the poly has more than two vertices on one line, removes the ones that are not on the edges

    def get_lines(self):
        lines = []
        vertices_len = len(self.vertices)
        for i in range(vertices_len):
            if i == vertices_len - 1:
                lines.append([self.vertices[i], self.vertices[0]])
            else:
                lines.append([self.vertices[i], self.vertices[i + 1]])
        return lines

    def to_contour(self):
        contour = []
        for vertex in self.vertices:
            x = vertex[0] * 10
            y = 247 - vertex[1] * 10
            contour.append([x, y])
            new_contour = np.array(contour, dtype=np.int32)
        return new_contour

    def intersects(self, line, is_in_line):
        # print(line)
        lines = []
        vertices_len = len(self.vertices)
        intersections_count = 0

        for i in range(vertices_len):
            if i == vertices_len - 1:
                lines.append([self.vertices[i], self.vertices[0]])
            else:
                lines.append([self.vertices[i], self.vertices[i + 1]])

        p1 = inter.Point(line[0][0], line[0][1])
        q1 = inter.Point(line[1][0], line[1][1])

        for poly_line in lines:
            if not is_in_line:
                if line[1] in poly_line or line[0] in poly_line:
                    continue

            p2 = inter.Point(poly_line[0][0], poly_line[0][1])
            q2 = inter.Point(poly_line[1][0], poly_line[1][1])

            if is_in_line:
                if line[1][1] == poly_line[0][1] or line[1][1] == poly_line[1][1]:
                    p2 = inter.Point(p2.a, p2.b + 0.1)
                    q2 = inter.Point(q2.a, q2.b + 0.1)

            if inter.do_intersect(p1, q1, p2, q2):
                intersections_count = intersections_count + 1

        intersections_count = intersections_count
        return intersections_count

    def in_lies(self, line):

        vertices = self.vertices
        max_x = 0

        for v in vertices:
            if v[0] > max_x:
                max_x = v[0]

        # get the center of the line
        vertex_center = [(line[0][0] + line[1][0]) / 2,
                         (line[0][1] + line[1][1]) / 2]
        # print(vertex_center)
        vertex_end = [max_x, vertex_center[1]]
        # extend the center point to the max x of the polygon
        vertical_line = [vertex_center, vertex_end]

        # print("vertical_line" + str(vertical_line))
        intersections_count = self.intersects(vertical_line, True)
        # print(intersections_count)

        if intersections_count % 2 == 0:
            return False
        else:
            return True

    def refine(self):
        vertices = self.vertices
        if len(vertices) <= 3:
            return self

        new_vertices = []
        start_vertex = vertices[0]
        next_vertex = vertices[1]
        prev_angle = calc_angle(start_vertex, next_vertex)
        # print(prev_angle)

        for j in range(1, len(vertices) - 1):
            start_vertex = vertices[j]
            next_vertex = vertices[j + 1]
            new_angle = calc_angle(start_vertex, next_vertex)
            # print(new_angle)

            if new_angle > prev_angle + REFINE_ANGLE_MARGIN or new_angle < prev_angle - REFINE_ANGLE_MARGIN:
                prev_angle = new_angle
                new_vertices.append(start_vertex)

            start_vertex = next_vertex

        # check if the first element and the last element are on the same line
        next_vertex = vertices[0]
        next_angle = calc_angle(start_vertex, next_vertex)
        # print(next_angle)
        if next_angle > prev_angle + REFINE_ANGLE_MARGIN or next_angle < prev_angle - REFINE_ANGLE_MARGIN:
            new_vertices.append(start_vertex)
            prev_angle = next_angle

        start_vertex = next_vertex
        next_vertex = vertices[1]
        next_angle = calc_angle(start_vertex, next_vertex)

        if next_angle > prev_angle + REFINE_ANGLE_MARGIN or next_angle < prev_angle - REFINE_ANGLE_MARGIN:
            new_vertices.append(start_vertex)

        new_poly = Polygon(new_vertices)
        return new_poly

    def is_rect(self):
        if len(self.vertices) == 4:
            line1 = calc_dist(self.vertices[0], self.vertices[1])
            line2 = calc_dist(self.vertices[1], self.vertices[2])
            line3 = calc_dist(self.vertices[2], self.vertices[3])
            line4 = calc_dist(self.vertices[3], self.vertices[0])

            diagonal1 = calc_dist(self.vertices[0], self.vertices[2])
            # print(diagonal1)
            diagonal2 = calc_dist(self.vertices[1], self.vertices[3])
            # print(diagonal2)

            expected_diagonal1 = math.sqrt(
                math.pow(line1, 2) + math.pow(line2, 2))
            expected_diagonal2 = math.sqrt(
                math.pow(line3, 2) + math.pow(line4, 2))
            # print(expected_diagonal1)
            # print(expected_diagonal2)

            if abs(expected_diagonal1 - diagonal1) < RECT_DIAGONAL_MARGIN and abs(
                    expected_diagonal2 - diagonal2) < RECT_DIAGONAL_MARGIN:

                return True
            else:
                return False

        else:
            return False

    def is_triangle(self):
        if len(self.vertices) == 3:
            return True
            # line1 = [self.vertices[0], self.vertices[1]]
            # line2 = [self.vertices[1], self.vertices[2]]
            # line3 = [self.vertices[0], self.vertices[2]]

            # angle1 = calc_lines_angle(line1, line2)
            # angle2 = calc_lines_angle(line2, line3)
            # angle3 = calc_lines_angle(line3, line1)

            # print(angle1)
            # print(angle2)
            # print(angle3)

            # if abs(angle1) + abs(angle2) + abs(angle3) == 180:
            # return True
            # else:
            # return False
        else:
            return False

    def is_circle(self):
        lines = []
        angles = []
        vertices_len = len(self.vertices)
        for i in range(vertices_len):
            if i == vertices_len - 1:
                lines.append([self.vertices[i], self.vertices[0]])
            else:
                lines.append([self.vertices[i], self.vertices[i + 1]])

        for i in range(vertices_len):
            if i == vertices_len - 1:
                angle = abs(calc_lines_angle(lines[i], lines[0]))
            else:
                angle = abs(calc_lines_angle(lines[i], lines[i + 1]))

            angle = math.ceil(angle)
            if angle >= 90:
                angle = angle - 90
            angles.append(angle)

        # check if all angles are equal
        # print("angle " + str(angles))
        for angle in angles:
            if angle > angles[0] + 1 or angle < angles[0] - 1:
                return False
        return True

    def is_circle_2(self):
        vertices = self.vertices
        max_x = 0
        max_y = 0
        min_x = 0
        min_y = 0

        for v in vertices:
            if v[0] > max_x:
                max_x = v[0]
            if v[0] < min_x:
                min_x = v[0]

            if v[1] > max_y:
                max_y = v[1]
            if v[1] < min_y:
                min_y = v[1]

        center_x = (max_x - min_x) / 2
        # print(center_x)
        center_y = (max_y - min_y) / 2
        # print(center_y)
        center_vertex = [center_x, center_y]

        dist_prev = calc_dist(center_vertex, vertices[0])
        # print(dist_prev)
        for i in range(1, len(vertices)):
            dist_next = calc_dist(center_vertex, vertices[i])
            # print(dist_next)
            if dist_next > dist_prev + CIRCLE_DST_MARGIN or dist_next < dist_prev - CIRCLE_DST_MARGIN:
                return False
                # print("False")

        return True

    def is_primitive(self):
        self = self.refine()
        vertices_len = len(self.vertices)

        if vertices_len < 3:
            return True
        if vertices_len == 3:
            return self.is_triangle()
        if vertices_len == 4:
            return self.is_rect()
        if vertices_len > 4:
            return self.is_circle_2()


if __name__ == "__main__":
    print(latex2poly(
        "(40.1, 23.6) --(40.1, 23.3) --(40.2, 23.2) --(39.9, 22.9) --(39.8, 22.9) --(39.8, 20.9) --(39.9, 20.8) --(39.9, 20.7) --(39.8, 20.6) --(39.8, 20.2) --(39.9, 20.1) --(39.7, 19.9) --(38.9, 19.9) --(38.9, 19.6) --(39.0, 19.5) --(38.8, 19.3) --(38.6, 19.3) --(38.6, 19.0) --(39.4, 19.0) --(39.5, 18.9) --(39.5, 19.4) --(39.7, 19.6) --(40.0, 19.6) --(40.2, 19.4) --(40.1, 19.3) --(40.1, 18.9) --(40.2, 18.8) --(40.0, 18.6) --(39.7, 18.6) --(39.6, 18.7) --(39.5, 18.7) --(39.4, 18.6) --(38.4, 18.6) --(38.4, 18.3) --(38.5, 18.2) --(38.3, 18.0) --(38.3, 17.8) --(38.4, 17.7) --(38.4, 17.4) --(38.3, 17.3) --(38.3, 17.1) --(38.6, 17.1) --(38.6, 18.0) --(38.5, 18.1) --(38.7, 18.3) --(43.9, 18.3) --(43.9, 18.6) --(43.5, 18.6) --(43.3, 18.8) --(43.3, 19.2) --(43.0, 19.2) --(43.0, 18.9) --(43.1, 18.8) --(42.9, 18.6) --(42.4, 18.6) --(42.2, 18.8) --(42.2, 19.2) --(42.1, 19.3) --(42.1, 19.4) --(42.3, 19.6) --(43.3, 19.6) --(43.3, 19.9) --(42.6, 19.9) --(42.4, 20.1) --(42.5, 20.2) --(42.5, 20.5) --(42.4, 20.4) --(41.6, 20.4) --(41.6, 20.1) --(41.4, 19.9) --(40.9, 19.9) --(40.7, 20.1) --(40.7, 20.7) --(40.6, 20.8) --(40.6, 21.3) --(40.7, 21.4) --(40.7, 23.1) --(40.9, 23.3) --(41.6, 23.3) --(41.8, 23.1) --(41.6, 22.9) --(41.6, 20.8) --(42.4, 20.8) --(42.4, 20.9) --(42.5, 21.0) --(42.5, 23.0) --(42.4, 23.0) --(42.2, 23.2) --(42.2, 23.6)"))
    l = [[38.4, 13.7], [44.0, 13.7]]
    test = [[40.7, 23.1], [40.9, 23.3], [41.6, 23.3], [41.8, 23.1], [41.6, 22.9], [41.6, 20.8], [42.4, 20.8], [42.4, 20.9], [
        42.5, 21.0], [42.5, 23.0], [42.4, 23.0], [42.2, 23.2], [42.2, 23.6], [40.1, 23.6], [40.1, 23.3], [40.2, 23.2]]

    test2 = [[40.9, 19.9], [40.7, 20.1], [40.7, 20.7], [40.6, 20.8], [40.6, 21.3], [40.7, 21.4], [40.7, 23.1], [
        40.2, 23.2], [39.9, 22.9], [39.8, 22.9], [39.8, 20.9], [39.9, 20.8], [39.9, 20.7], [39.8, 20.6], [39.8, 20.2]]

    test3 = [[40.0, 18.6], [39.7, 18.6], [39.6, 18.7], [39.5, 18.7], [39.4, 18.6], [38.4, 18.6], [38.4, 18.3], [38.5, 18.2], [38.3, 18.0], [
        38.3, 17.8], [38.4, 17.7], [38.4, 17.4], [38.3, 17.3], [38.3, 17.1], [38.6, 17.1], [38.6, 18.0], [38.5, 18.1], [38.7, 18.3]]
    test4 = [[38.7, 18.3], [43.9, 18.3], [43.9, 18.6], [43.5, 18.6], [43.3, 18.8], [43.3, 19.2], [43.0, 19.2], [43.0, 18.9], [43.1, 18.8], [42.9, 18.6], [42.4, 18.6], [42.2, 18.8], [42.2, 19.2], [42.1, 19.3], [42.1, 19.4], [42.3, 19.6], [43.3, 19.6], [43.3, 19.9], [42.6, 19.9], [42.4, 20.1], [42.5, 20.2], [42.5, 20.5], [42.4, 20.4], [
        41.6, 20.4], [41.6, 20.1], [41.4, 19.9], [40.9, 19.9], [39.8, 20.2], [39.9, 20.1], [39.7, 19.9], [38.9, 19.9], [38.9, 19.6], [39.0, 19.5], [38.8, 19.3], [38.6, 19.3], [38.6, 19.0], [39.4, 19.0], [39.5, 18.9], [39.5, 19.4], [39.7, 19.6], [40.0, 19.6], [40.2, 19.4], [40.1, 19.3], [40.1, 18.9], [40.2, 18.8], [40.0, 18.6]]

    test_poly = Polygon(test4)
    print(test_poly.refine().is_circle_2())
