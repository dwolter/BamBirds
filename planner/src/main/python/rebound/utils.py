import logging
import math
import copy
import numpy as np
from . import settings
import PIL.Image as Image

log = logging.getLogger("rebound.utils")


def cw(i):
    """The index of the next edge clockwise

    Arguments:
        i {int} -- edge

    Returns:
        int -- new edge
    """
    return (i-1) % 4


def ccw(i):
    """The index of the next edge counterclockwise

    Arguments:
        i {int} -- edge

    Returns:
        int -- new edge
    """
    return (i+1) % 4


def par(i):
    """The index of the parallel edge

    Arguments:
        i {int} -- edge

    Returns:
        int -- new edge
    """
    return (i+2) % 4


def possible_angles(_from, _to):
    """the possible angles when coming from index _from and going to index _to

    Arguments:
        _from {int} -- in edge
        _to {int} -- out edge

    Returns:
        list -- possible angles
    """
    start = income_angle(_from)
    if _to == cw(_from):
        return [start, bound_angle(start+90)]
    elif _to == par(_from):
        return [bound_angle(start-45), bound_angle(start+45)]
    elif _to == ccw(_from):
        return [bound_angle(start-90), start]
    elif _to == _from:
        return [bound_angle(start+90), bound_angle(start-90)]


def income_angle(i):
    """Income angle for an edge

    Arguments:
        i {int} -- edge

    Returns:
        int -- income angle
    """
    return bound_angle(int(i) % 4 * 90)


def bound_angle(angle):
    """returns the angle bounded between 0 and 360 degrees

    Arguments:
        angle {Number} -- Input angle (in degrees)

    Returns:
        float -- Angle in range [0,360[
    """
    return angle % 360


def between(a, b, ordered=False, bound=True, signed=False):
    """Angle between two angles (which start from the x axis)

    Arguments:
        a (Number): angle A (in degrees)
        b (Number): angle B (in degrees)

    Returns:
        Number: the angle between a and b (in degrees)
    """
    if ordered and b < a:
        a -= 360
    angle = b - a
    if not signed:
        angle = abs(angle)
    if bound:
        angle = bound_angle(angle)
    return angle

def mean_angle(a,b):
    """Mean angle of an angle interval

    If interval s looping, it is made non looping before calculating the mean

    Arguments:
        a {float} -- start of interval
        b {float} -- end of interval

    Returns:
        float -- mean of the interval
    """
    if between(a, b) > 180:
        a += 360
    return np.mean([a,b])

def unit_vector(vector) -> np.ndarray:
    """ Returns the unit vector of the vector.  """
    return vector / np.linalg.norm(vector)

def vector_to_angle(v):
    """Calculate the angle for a vector

    Arguments:
        v {iterable} -- vector

    Returns:
        float -- angle of the vector
    """
    return math.degrees(math.atan2(v[1],v[0]))

def vector_angle_between(v1, v2, ordered=True, signed=True, bound=True):
    """Returns the angle in degrees between vectors 'v1' and 'v2'

    Arguments:
        v1 {iterable} -- first vector
        v2 {iterable} -- second vector

    Keyword Arguments:
        signed {bool} -- if the angle should be signed
        bound {bool} -- if the angle should be bound between 0 and 360 degrees (default: {True})

    Returns:
        float -- Angle in degrees between v1 and v2.

    References:
        https://www.mathworks.com/matlabcentral/answers/266282-negative-angle-between-vectors-planes
        https://stackoverflow.com/questions/2827393/angles-between-two-n-dimensional-vectors-in-python/13849249#13849249
    """
    a1 = vector_to_angle(v1)
    a2 = vector_to_angle(v2)
    return between(a1, a2, ordered=ordered, bound=bound, signed=signed)


def calc_bounce_angles(P, rg_start, v_start, angle_wall):
    """Calculate the angles after a bounce

    Arguments:
        P {tuple} -- Coordinates of the bounce Point (only x is used and no check is performed if parabola passes P)
        rg_start {list} -- range of angles at 0,0
        v_start {float} -- velocity at start point
        angle_wall {list} -- normal vector of the wall

    Returns:
        tuple -- tuple of the new angles and velocity after the bounce

    References:
        https://stackoverflow.com/questions/573084/how-to-calculate-bounce-angle
    """
    rg_average = mean_angle(*rg_start)


    # FIXME: distance between the start angles might not be the same as at the bounce Point
    current_angles = current_angle(rg_start,v_start,[P,P])
    rg_dist = between(current_angles[0], current_angles[1]) / 2
    if settings.bounce_variance_relative:
        rg_dist *= (1 + settings.bounce_variance)
    else:
        rg_dist += settings.bounce_variance

    v_current = np.array(current_velocity(
        P[0], v_start, math.radians(rg_average), components=True))

    if vector_angle_between(v_current,angle_wall,signed=False) < 90 - rg_dist:
        raise ValueError("Cannot bounce inside of a wall")

    angle_wall = unit_vector(angle_wall)

    u = (v_current.dot(angle_wall)) * angle_wall
    w = v_current - u

    v_new = settings.friction * w - settings.restitution * u

    rg_new_average = vector_angle_between((1, 0), v_new)

    # use the distance to the wall to make sure the new angles with variance are not inside the wall
    distance_to_wall = vector_angle_between(
        w, v_new, ordered=False, bound=False, signed=False)
    rg_new = [
        bound_angle(rg_new_average - min(rg_dist, distance_to_wall)),
        # The second one should actually never be inside the wall (edge cases?) unless the bounce variance is too high
        bound_angle(rg_new_average + min(rg_dist, 180 - distance_to_wall))
    ]

    return rg_new, components_to_velocity(v_new)

def calc_sliding_velocity(P, v, theta, gravity=settings.gravity):
    """!!! Warning not tested !!!

    Calculate the velocity for sliding

    Arguments:
        P {tuple} -- relative point to origin for new velocity
        v {float} -- current velocity
        theta {float} -- angle of the motion

    Keyword Arguments:
        gravity {float} -- gravitational force (default: {settings.gravity})

    Returns:
        float -- new velocity
    """
    radiant = math.radians(theta)
    sin = math.sin(radiant)
    tangent = math.tangent(theta)
    if sin > 0:
        return v + math.sqrt(gravity*P[0]*(tangent - (1 - settings.friction)))
    else:
        log.warn("Rolling uphill not implemented yet")

def interval_intersection(range_a, range_b, is_angle=True):
    """Calculate the intersection of intervals 

    Arguments:
        range_a {iterable} -- first interval
        range_b {iterable} -- second interval

    Keyword Arguments:
        is_angle {bool} -- if the intervals are angles (with looping values) (default: {True})

    Raises:
        ArithmeticError: if there is no intersection

    Returns:
        list -- intersection of the intervals
    """
    log.debug("%s,%s", range_a, range_b)
    
    # Transform angle ranges to non looping ones (makes calculations more readable)
    if is_angle:
        range_a, range_b = angle_intervals_closer(range_a, range_b)

    # if the ranges don't overlap, raise Error
    if not intervals_intersect(range_a,range_b):
        raise ArithmeticError(
            "No range overlap with {} and {}".format(range_a, range_b))

    Θ_1 = max(range_a[0], range_b[0])
    Θ_2 = min(range_a[1], range_b[1])

    if is_angle:
        return [bound_angle(Θ_1), bound_angle(Θ_2)]
    else:
        return [Θ_1, Θ_2]


def interval_union(range_a, range_b, is_angle=True):
    """Calculate the union of two intervals

    Arguments:
        range_a {iterable} -- first interval
        range_b {iterable} -- second interval

    Keyword Arguments:
        is_angle {bool} -- if the intervals are angles (with looping values) (default: {True})

    Raises:
        ArithmeticError: if there is no intersection between the intervals

    Returns:
        list -- union of the intervals
    """
    # Transform angle ranges to non looping ones (makes calculations more readable)
    log.debug("%s,%s", range_a, range_b)

    if is_angle:
        range_a, range_b = angle_intervals_closer(range_a, range_b)

    # if the ranges don't overlap, raise Error
    if not intervals_intersect(range_a,range_b):
        raise ArithmeticError(
            "No range overlap with {} and {}".format(range_a, range_b))

    Θ_1 = min(range_a[0], range_b[0])
    Θ_2 = max(range_a[1], range_b[1])

    if is_angle:
        return [bound_angle(Θ_1), bound_angle(Θ_2)]
    else:
        return [Θ_1, Θ_2]


def angle_intervals_closer(interval_a, interval_b):
    """Move angle intervals closer to each other and make then not looping

    Arguments:
        interval_a {iterable} -- first interval
        interval_b {iterable} -- second interval

    Returns:
        tuple -- (interval_a, interval_b) but closer to each other
    """
    α_1 = interval_a[0]
    β_1 = interval_b[0]
    α_2 = interval_a[1] + 360 if interval_a[1] < α_1 else interval_a[1]
    β_2 = interval_b[1] + 360 if interval_b[1] < β_1 else interval_b[1]

    if β_2 < α_1:
        β_1 += 360
        β_2 += 360
    elif β_1 > α_2:
        α_1 += 360
        α_2 += 360

    return ([α_1, α_2],[β_1,β_2])


def intervals_intersect(interval_a, interval_b, adapt_angles=False):
    """Check if the angle intervals intersect

    Arguments:
        interval_a {iterable} -- first interval
        interval_b {iterable} -- second interval

    Keyword Arguments:
        adapt_angles {bool} -- if the angles should first be moved closer to each other before checking the intersection (default: {False})

    Returns:
        bool -- if the intervals itersect
    """
    if adapt_angles:
        interval_a, interval_b = angle_intervals_closer(interval_a, interval_b)
    return not (interval_a[0] > interval_b[1] or interval_b[0] > interval_a[1])


def in_angle_interval(value, interval):
    """Check if the value is inside the angle interval

    Arguments:
        value {float} -- value to check
        interval {iterable} -- interval

    Returns:
        bool -- if the value is in the angle interval
    """
    α_1 = interval[0]
    α_2 = interval[1] + 360 if interval[1] < α_1 else interval[1]
    return α_1 <= value and value <= α_2


def in_interval(value, interval, reorganize_range=True):
    """Check if a value is inside a interval

    Arguments:
        value {float} -- value to check
        interval {iterable} -- interval to check

    Keyword Arguments:
        reorganize_range {bool} -- if the values should be flipped if they are not in the right order (default: {True})

    Returns:
        bool -- if the value is inside the interval
    """
    r = copy.deepcopy(interval)
    value = round(value, 7)
    if r[0] > r[1]:
        r.reverse()
    return r[0] <= value and value <= r[1]


def start_angle(Points, angle_interval, velocity=None, gravity=settings.gravity):
    """Calculate the start angle of the parabola from O to P

    Arguments:
        Points {iterable} -- Points relative to the Origin, if velocity is set, needs to be the the edges endpoints
        angle_interval {iterable} -- Current angle interval at Point P

    Keyword Arguments:
        velocity {float} -- starting velocity. if set, the points are used to check if the calculated value goes through the edge (default: {None})
        gravity {float} -- gravitational force (default: {settings.gravity})

    Raises:
        ValueError: if the velocity is not high enough to hit the edge or the starting angle with the velocity does not go through the edge

    Returns:
        iterable -- interval for the starting angles
    """
    log.debug("- start_angle")
    result = []

    for P, theta in zip(Points, angle_interval):
        angle_dir = horizontal_direction(theta)
        point_vertical = math.isclose(P[0], 0)
        angle_vertical = math.isclose(angle_dir, 0.0, abs_tol=1e-5)
        if point_vertical:
            # Move the Point to the middle of the edge depending on the cosine of the angle
            P = (P[0] + angle_dir * 0.5 * settings.edge_length, P[1])
        if angle_vertical and point_vertical:
            theta_0 = theta
        elif angle_vertical and not point_vertical:
            raise ValueError("Point is not vertical but angle was")
        else:
            log.debug("%s %f", P, theta)
            radians = math.radians(theta)
            if not velocity:
                m = math.tan(radians)
                a = (-P[1]+m * P[0])/(P[0]**2)
                if a > 0:
                    raise ValueError(
                        "angle is not achievable with positive gravity")
                b = m - 2 * a * P[0]

                theta_0 = math.degrees(math.atan(b))
            else:
                thetas = []
                for i in [-1, 1]:
                    try:
                        thetas.append(math.degrees(math.atan(
                            (
                                velocity**2 + i * math.sqrt(
                                    velocity**4 - 4 * gravity * P[0] * (
                                        gravity * P[0] + velocity**2 *
                                        math.tan(radians)
                                    )
                                )
                            ) /
                            (
                                2 * gravity * P[0]
                            )
                        )))
                    except (ValueError, ZeroDivisionError) as e:
                        continue
                if not thetas:
                    raise ValueError(
                        "angle is not achievable with positive gravity and the given velocity")
                else:
                    for theta_opt in thetas:
                        if hits_edge(theta_opt, velocity, Points):
                            # It should only be possible to hit the same Point at the same angle if the angle is the minimal angle to hit it.
                            # Then it should be irrelevant if one or the other is used
                            theta_0 = theta_opt
                            break
                    try:
                        theta_0 = theta_0
                    except NameError as e:
                        raise ValueError(
                            "the only possible angles do not hit the edge")

            if angle_dir < 0:
                theta_0 += 180

        result.append(bound_angle(theta_0))

    return result


def start_velocity(Points, angle_0, gravity=settings.gravity):
    """Calculate the minimum start velocity for the given start angles and Points

    Arguments:
        Points {iterable} -- Points to reach (relative to the starting point in pixel coordinates, i.e. P(20,-50) for S(500,350), T(520,300))
        angle_0 {iterable} -- starting angles

    Keyword Arguments:
        gravity {float} -- Gravitational force in pixel/s^2 (default: {settings.gravity})

    Raises:
        ValueError: when there is no solution

    Returns:
        iterable -- The start velocities for each angle_0
    """
    log.debug("- start_velocity")
    result = []

    for P, theta_0 in zip(Points, angle_0):
        log.debug("%s %f", P, theta_0)
        radiant = math.radians(theta_0)
        log.debug("radiant: %f", radiant)

        # Handle 90 and 270 degrees separately because they cannot be calculated with the standard formula
        # If the relative Point for 90 / 270 degrees is not vertically displaced, raise an Error
        if math.isclose(horizontal_direction(theta_0), 0.0, abs_tol=1e-7):
            if math.isclose(P[0], 0, abs_tol=0.2):
                # The minimum velocity function can also handle vertical relative Points
                v_0 = minimum_velocity(P, gravity)
            else:
                raise ValueError(
                    "Point is not vertical but angle was 90 degrees")
        else:
            calc = (gravity*P[0]**2)/(2*math.cos(radiant)
                                      ** 2*(math.tan(radiant)*P[0]-P[1]))

            log.debug("({}x{}^2)/(2x{}x(-{}+{}x{}))={}".format(gravity,
                                                               P[0], math.cos(radiant), P[1], math.tan(radiant), P[0], calc))
            v_0 = math.sqrt(calc)
        result.append(v_0)
    return result


def current_angle(angle_interval, v_0, Points, gravity=settings.gravity):
    """Calculates current angle at relative x-coordinate given start-angle and -velocity

    Arguments:
        angle_interval {iterable} -- starting angle interval
        v_0 {float} -- starting velocity
        Points {iterable} -- points relative to start-point

    Keyword Arguments:
        gravity {float} -- Gravitational force in pixel/s^2 (default: {settings.gravity})

    Raises:
        ValueError: if the angle can not be calculated

    Returns:
        iterable -- current angles for the respective velocities and angles
    """
    log.debug("- current_angle")
    result = []
    for theta_0, P in zip(angle_interval, Points):
        theta = theta_0
        if math.isclose(theta_0, 90, abs_tol=1e-7):
            if hits_edge(theta_0, v_0, Points):
                if start_velocity([P], [theta_0])[0] < v_0:
                    theta = 90
                else:
                    theta = 270
            else:
                raise ValueError(
                    "Point is not vertical but starting angle was 90 degrees")
        elif math.isclose(theta_0, 270, abs_tol=1e-7):
            if hits_edge(theta_0, v_0, Points):
                theta = 270
            else:
                raise ValueError(
                    "Point is not vertical but starting angle was 270 degrees")
        else:
            if v_0 <= 0:
                raise ValueError("velocity can not be less or equal to 0")
            radiant = math.radians(theta_0)
            log.debug("radiant: %f", radiant)
            theta = math.atan(math.tan(radiant) -
                              (gravity * P[0])/(v_0**2*math.cos(radiant)**2))

            if horizontal_direction(theta_0) < 0:
                theta -= math.pi
            log.debug("theta: %f %f", theta, math.degrees(theta))
            theta = math.degrees(theta)
        result.append(bound_angle(theta))
    if not correct_angle_order(result):
        result.reverse()
    return result

def direction(angle):
    """calculates the direction a angle is going to

    Arguments:
        angle {float} -- an angle in degrees counterclockwise

    Returns:
        tuple -- (-|~0|+,-|~0|+) for (left|vertical|right,down|horizontal|up)
    """
    radiant = math.radians(angle)
    return math.cos(radiant), math.sin(radiant)

def horizontal_direction(angle):
    """calculates the vertical direction a angle is going to

    Arguments:
        angle {float} -- an angle in degrees counterclockwise

    Returns:
        float -- -|~0|+ for (left|vertical|right,down|horizontal|up)
    """
    return math.cos(math.radians(angle))

def vertical_direction(angle):
    """calculates the direction a angle is going to

    Arguments:
        angle {float} -- an angle in degrees counterclockwise

    Returns:
        float -- -|~0|+ for down|horizontal|up
    """
    return math.sin(math.radians(angle))

def convert_coordinates(P):
    """Converts pixel coordinates to euclidean coordinates (and back)

    Arguments:
        P {iterable} -- Point in x,y coordinates

    Returns:
        list -- Point with y coordinate flipped 
    """
    return [P[0], -P[1]]


def edge_coordinates(P, edge):
    """returns the coordinates of the middle of the edge for pixel P

    Arguments:
        P {iterable} -- a zone
        edge {int} -- the edge number (0 is left, then clockwise to 3. Numbers are used with mod)

    Returns:
        list -- [description]
    """
    edge = edge % 4
    margin = 0.5*settings.edge_length
    if edge == 0:
        return [P[0], P[1]+margin]
    elif edge == 1:
        return [P[0]+margin, P[1]+2*margin]
    elif edge == 2:
        return [P[0]+2*margin, P[1]+margin]
    else:
        return [P[0]+margin, P[1]]


def neighbours (P):
    return [
        ((P[0], P[1]+settings.edge_length), settings.edge_length),
        ((P[0]+settings.edge_length, P[1]+settings.edge_length), settings.diagonal_zone_to_zone),
        ((P[0]+settings.edge_length, P[1]), settings.edge_length),
        ((P[0]+settings.edge_length, P[1]-settings.edge_length), settings.diagonal_zone_to_zone),
        ((P[0], P[1]-settings.edge_length), settings.edge_length),
        ((P[0]-settings.edge_length, P[1]-settings.edge_length), settings.diagonal_zone_to_zone),
        ((P[0]-settings.edge_length, P[1]), settings.edge_length),
        ((P[0]-settings.edge_length, P[1]+settings.edge_length), settings.diagonal_zone_to_zone),
    ]


def calculation_points(O, P, edge):
    """Generates relative edge Points

    Arguments:
        O {iterable} -- coordinates of the origin edge
        P {iterable} -- coordinates of the edge
        edge {int} -- number of the edge, multiples of 2 are vertical (including 0)

    Returns:
        iterable -- Points for the edge, they are ordered in a way such that the first one will result in a lower value angle
    """
    O = convert_coordinates(O)
    P = convert_coordinates(P)

    result = []
    P_local = relative_point(O, P)

    margin = 0.5*settings.edge_length

    if edge % 2 == 0:
        result.append(
            [P_local[0], P_local[1]-math.copysign(margin, P_local[0])])
        result.append(
            [P_local[0], P_local[1]+math.copysign(margin, P_local[0])])
    else:
        # For the horizontal edges we need the Point further to the right in x direction first
        result.append([P_local[0]+margin, P_local[1]])
        result.append([P_local[0]-margin, P_local[1]])
    return result


def relative_point(O, P):
    return [p-o for o, p in zip(O, P)]


def minimum_velocity(P, gravity=settings.gravity):
    """Calculate minimum velocity to reach Point P

    Arguments:
        P {iterable} -- coordinates of Point

    Keyword Arguments:
        gravity {float} -- Gravitational force (default: {9.81})

    Returns:
        float -- minimum velocity required to reach the Point
    """
    return math.sqrt(gravity*(P[1]+math.sqrt(P[1]**2+P[0]**2)))


def current_velocity(x, v, a, g=settings.gravity, components=False):
    """Calculate the velocity at the given x

    Arguments:
        x {float} -- x coordinate
        v {float} -- start velocity
        a {float} -- angle in radians

    Keyword Arguments:
        g {float} -- Gravitational force (default: {settings.gravity})

    Returns:
        float -- velocity at the given x coordinate
    """
    v_y = v * math.sin(a) - g * time_point(x, v, a)
    v_x = v*math.cos(a)
    if components:
        return v_x, v_y
    return components_to_velocity([v_x, v_y])


def components_to_velocity(v):
    return math.sqrt(v[0]**2 + v[1]**2)


def time_point(x, v, a):
    """Calculate the time passed until the x coordinate of P is reached

    Arguments:
        x {float} -- relative coordinates (i.e. origin of parabola is at (0,0))
        v {float} -- velocity
        a {float} -- angle (in radians)

    Returns:
        {float} -- time
    """
    return x / (v * math.cos(a))


def projectile_trajectory(x, theta_0, v_0, gravity=settings.gravity, O=[0, 0]):
    """Function for the trajectory of a Projectile

    Arguments:
        x {int} -- x coordinate
        theta_0 {float} -- starting angle (in radians)
        v_0 {float} -- starting velocity

    Returns:
        float -- y value at the given x

    Raises:
        ValueError -- if the angle is vertical, i.e. 90 or 270 degrees
        ZeroDevisionError -- if the velocity is 0
    """
    cosine = math.cos(theta_0)
    x = x-O[0]
    if math.isclose(cosine, 0.0, abs_tol=1e-7):
        raise ValueError(
            "Projectile trajectory not defined with x when angle is vertical")
    return (math.tan(theta_0) * x - (gravity * x**2)/(2 * v_0**2 * cosine**2))+O[1]


def inversed_projectile_trajectory(y, theta_0, v_0, gravity=settings.gravity, O=[0, 0]):
    """Inverse Function for the trajectory of a Projectile

    Arguments:
        x {int} -- x coordinate
        theta_0 {float} -- starting angle (in radians)
        v_0 {float} -- starting velocity

    Returns:
        tuple -- two possible x values for the given y

    Raises:
        ValueError -- if the angle is vertical, i.e. 90 or 270 degrees or if y value is too high
        ZeroDevisionError -- if the velocity is 0
    """
    cosine = math.cos(theta_0)
    if math.isclose(cosine, 0.0, abs_tol=1e-7):
        raise ValueError(
            "Projectile trajectory not defined with x when angle is vertical")
    tangent = math.tan(theta_0)
    velocity_component = gravity / (v_0**2 * cosine**2)
    sqrt = math.sqrt(tangent**2 - 2 * velocity_component * (y - O[1]))
    x_1 = (tangent + sqrt) / velocity_component
    x_2 = (tangent - sqrt) / velocity_component
    return x_1 + O[0], x_2 + O[0]


def hits_edge(theta, v, Points, gravity=settings.gravity):
    """Determines if the given values for the range actually hit the edge defined by Points

    Arguments:
        rg {iterable} -- Sequence of the format [angle, velocity]. Angle in degrees
        Points {iterable} -- two points containing the coordinates of the edge

    Keyword Arguments:
        gravity {float} -- Gravitational force (default: {9.81})

    Returns:
        bool -- if the point is reached with a tolerance of 0.6 pixels in the y direction
    """
    # Middle point for the edge
    P = np.average(Points, 0)
    theta_direction = direction(theta)
    if math.isclose(theta_direction[0], 0, abs_tol=1e-7):
        # return true iff Point is vertical, velocity is high enough and the direction is correct
        return math.isclose(P[0], 0, abs_tol=0.2) and minimum_velocity(P) <= v and math.copysign(1, theta_direction[1]) == math.copysign(1, P[1])
    else:
        if Points[0][0] != Points[1][0] and Points[0][1] == Points[1][1]:
            # TODO: maybe use a inverted function to get the x value(s) for a given y value
            x_1, x_2 = inversed_projectile_trajectory(
                P[1], math.radians(theta), v)
            x_range = [Points[0][0], Points[1][0]]
            return in_interval(x_1, x_range, reorganize_range=True) or in_interval(x_2, x_range, reorganize_range=True)
        else:
            y = projectile_trajectory(P[0], math.radians(theta), v)
            return in_interval(y, [Points[0][1], Points[1][1]], reorganize_range=True)
            # if Points[0][1] < Points[1][1]:
            #     return Points[0][1] <= y and y <= Points[1][1]
            # else:
            #     return Points[1][1] <= y and y <= Points[0][1]


def angle_to_hit(v_0, P, is_higher, gravity=settings.gravity):
    """Like start_angle but uses velocity rather than current_angle

    Arguments:
        v_0 {float} -- start velocity
        P {iterable} -- coordinates of the relative point to reach
        is_higher {bool} -- if the higher or lower angle is wanted

    Keyword Arguments:
        gravity {float} -- gravitational force (default: {9.81})

    Returns:
        float -- the angle required to hit in degrees

    Raises:
        ValueError -- if the velocity is not high enough to reach the point
        ZeroDivisionError -- if the x-coordinate of P is 0
    """
    degree_adjust = math.pi if P[0] < 0 else 0
    i = 1 if is_higher else -1
    if math.isclose(P[0], 0, abs_tol=1e-7):
        if is_higher:
            return 90
        else:
            return 90 if P[1] > 0 else 270
    return bound_angle(math.degrees(
        math.atan(
            (v_0**2 + i * math.sqrt(
                v_0**4 - gravity * (gravity * P[0]**2 + 2 * P[1] * v_0**2)
            )
            ) /
            (gravity * P[0])
        ) - degree_adjust
    ))


def angles_to_hit_edge(velocity, edge_points, is_high, gravity=settings.gravity):
    """Calculate the min and max angle for hitting the edge defined by edge_points

    Arguments:
        velocity {float} -- the velocity of the projectile in the Origin Point
        edge_points {iterable} -- Points defining the edge
        is_high {bool} -- If the higher or lower angle should be calculated

    Keyword Arguments:
        gravity {float} -- gravitational force (default: {settings.gravity})

    Raises:
        ValueError: if the velocity does not reach the edge

    Returns:
        iterable -- angle interval
    """
    copied_edge_points = copy.deepcopy(edge_points)
    angles = []
    if copied_edge_points[0][0] == copied_edge_points[1][0]:
        # vertical edge
        for point in copied_edge_points:
            if minimum_velocity(point) > velocity:
                new_y = maximum_y(point[0], velocity)
                if in_interval(new_y, [copied_edge_points[0][1], copied_edge_points[1][1]], reorganize_range=True):
                    point[1] = new_y
                else:
                    raise ValueError(
                        "The given velocity does not hit the edge")
            angles.append(angle_to_hit(velocity, point, is_high))
    elif copied_edge_points[0][1] == copied_edge_points[1][1]:
        # horizontal edge
        for point in copied_edge_points:
            if minimum_velocity(point) > velocity:
                new_x = math.copysign(maximum_x(point[1], velocity), point[0])
                if in_interval(new_x, [copied_edge_points[0][0], copied_edge_points[1][0]], reorganize_range=True):
                    point[0] = new_x
                else:
                    raise ValueError(
                        "The given velocity does not hit the edge")
            angles.append(angle_to_hit(velocity, point, is_high))
    else:
        angles = [angle_to_hit(velocity, point, is_high)
                  for point in copied_edge_points]
    if not correct_angle_order(angles):
        angles.reverse()
    return angles


def maximum_x(y, v, g=settings.gravity):
    """Calculate the maxmimally possible reachable x at height y with velocity v

    Arguments:
        y {int} -- height
        v {float} -- velocitz

    Keyword Arguments:
        g {float} -- gravitational force (default: {settings.gravity})

    Returns:
        float -- maximally reachable x
    """
    v_2 = v**2
    return round_decimals_down(math.sqrt((v_2**2 - 2*g*y*v_2)/(g**2)), 5)


def maximum_y(x, v, g=settings.gravity):
    """Calculate the maxmimally possible reachable y at distance x with velocity v

    Arguments:
        x {int} -- distance
        v {float} -- velocity

    Keyword Arguments:
        g {float} -- gravitational force (default: {settings.gravity})

    Returns:
        float -- maxmimally possible reachable y
    """
    v_2 = v**2
    return round_decimals_down((v_2**2 - x**2*g**2)/(2*g*v_2), 5)


def is_higher(angle_a, angle_b):
    """Determine if angle a is higher than b in terms of starting angle for the trajectory

    Arguments:
        angle_a {Number} -- angle a
        angle_b {Number} -- angle b

    Returns:
        bool -- if angle a is higher than b
    """
    angle_a_dir = direction(angle_a)
    angle_b_dir = direction(angle_b)

    if math.copysign(1, angle_a_dir[0]) == math.copysign(1, angle_b_dir[0]):
        return angle_a_dir[1] > angle_b_dir[1]
    else:
        raise ValueError("Angles are not on the same side of the y axis")


def correct_angle_order(angles):
    """determine if the two angles are in the correct order, assuming they cannot be more than 180 degrees apart

    Arguments:
        angles {sequence} -- Two angles to test

    Returns:
        bool -- True if the second angle is bigger, else False
    """
    difference = angles[1] - angles[0]
    if abs(difference) > 180:
        a_0 = angles[0]-360 if angles[0] > 180 else angles[0]
        a_1 = angles[1]-360 if angles[1] > 180 else angles[1]
        difference = a_1 - a_0
    return difference > 0


def vertical_angle_magnitude(angle):
    radiant = math.radians(angle)
    return math.sin(radiant)*math.copysign(1, math.cos(radiant))


def edge_angles(angles: list):
    """Calculate the lowest and highest angles of a number of angles

    Arguments:
        angles {list} -- different angles. Can not span more than 180 degrees or the lowest and highest angle can not be calculated

    Returns:
        list -- lowest and highest angles
    """
    for i, angle_i in enumerate(angles):
        for j, angle_j in enumerate(angles):
            if abs(angle_i - angle_j) >= 180:
                if angle_i > angle_j:
                    angles[i] -= 360
                    break
                else:
                    angles[j] -= 360

    return [bound_angle(min(angles)), bound_angle(max(angles))]


def out_of_image(coords, lower_bound=False):
    """Determine if the coordinates are out of the image

    Arguments:
        coords {tuple} -- coordinates in the image

    Keyword Arguments:
        lower_bound {bool} -- if the ground should also be considered as a bound (default: {False})

    Returns:
        bool -- if the coordinates are out of the image
    """
    if coords[0] < 0 or coords[0] >= settings.img_width or coords[1] < 0:
        return True
    if lower_bound and coords[1] >= settings.img_height:
        return True
    return False


def shrink_resolution(pixels: np.ndarray, edge_length = settings.edge_length) -> np.ndarray:
    """Shrink the resolution to the edge_length

    Arguments:
        pixels {ndarray} -- pixel array with dimensions 

    Keyword Arguments:
        edge_length {int} -- [description] (default: {settings.edge_length})

    Returns:
        ndarray -- [description]
    """
    result = np.copy(pixels)
    width, height = pixels.shape[:2]
    for x in range(0, width, edge_length):
        for y in range(0, height, edge_length):
            a = pixels[x:min(x+edge_length,width), y:min(y+edge_length, height)]
            a = a.reshape((a.shape[0]*a.shape[1], a.shape[2]))
            uniques, counts = np.unique(a, return_counts=True, axis=0)
            most_likely = np.argmax(counts)
            most_likely_type,a_x,a_y = uniques[most_likely]
            # Search first occurence where a_x or a_y is not zero
            while a_x == 0 and a_y == 0 and counts[most_likely] > 0:
                other_type,a_x,a_y = uniques[most_likely]
                if most_likely_type == 0 and other_type != 0:
                    most_likely_type = other_type
                counts[most_likely] = 0
                most_likely = np.argmax(counts)
            result[x:min(x+edge_length,width), y:min(y+edge_length, height)] = [most_likely_type, a_x, a_y]
    return result


def check_zone(pixels, coords, index: (int, list), function):
    """Check the zone at given coodinates for the values

    Arguments:
        pixels {numpy.ndarray} -- Access to Pixels
        coords {iterable} -- coordinates of the pixel to check
        index {int, list} -- index(es) of the pixel value to check
        function {function} -- function to execute on pixels. First result of the function not 0 is returned


    Raises:
        IndexError: if coordinates are out of bounds

    Returns:
        any -- result depends on the function
    """
    if out_of_image(coords):
        raise IndexError("Out of bounds")
    if coords[1] >= settings.img_height:
        return function(np.array((settings.ABType.Ground, 127, 0))[index])

    return function(pixels[coords][index])


def create_distance_matrix(pixels, point_list, distances, img_width=settings.img_width, img_height=settings.img_height, edge_length=settings.edge_length) -> np.ndarray:
    """Creates a distance matrix for a state list 

    Arguments:
        pixels {numpy.ndarray} -- access to pixels
        point_list {iterable} -- list of points for the states
        distances {iterable} -- distances for each point to the target 

    Keyword Arguments:
        img_width {int} -- image width (default: {settings.img_width})
        img_height {int} -- image height (default: {settings.img_height})
        edge_length {int} -- edge length for the zones (default: {settings.edge_length})

    Returns:
        np.ndarray -- distance matrix for the state list
    """
    distance_heuristic_matrix = np.zeros(
        (img_width, img_height), dtype=np.float32)

    for x in range(0, img_width, edge_length):
        for y in range(0, img_height, edge_length):
            min_index = 0
            min_distance = np.inf
            # TODO: Would be nice set walls with infinite distance.
            # But we can only set individual pixels and not ranges because otherwise a big portion is skipped
            # if not check_zone(pixels, (x,y),0,lambda x: x == settings.ABType.Hill.value):
            for i, point in enumerate(point_list):
                distance = math.dist((x, y), point)
                if distance < min_distance:
                    # TODO: Would be nice to use check if there is a hill between the coordinates
                    # However it explodes the time to calculate
                    # if not hill_between(pixels, (x,y),point):
                    min_index = i
                    min_distance = distance
            min_distance = math.dist(
                (x, y), point_list[min_index]) + distances[min_index]
            # set range of values through slicing
            distance_heuristic_matrix[x:min(
                x+edge_length, img_width), y:min(y+edge_length, img_height)] = min_distance
    return distance_heuristic_matrix


def merge_intervals(intervals: list) -> list:
    """Merge multiple angle intervals

    Arguments:
        intervals {list} -- intervals to merge

    Returns:
        list -- Merged intervals
    
    References
        https://stackoverflow.com/questions/43600878/merging-overlapping-intervals
    """
    intervals.sort(key=lambda interval: interval[0])
    merged = [intervals[0]]

    for current in intervals[1:]:
        previous = merged[-1]
        try:
            overlapped = interval_union(previous, current)
            merged[-1] = overlapped
        except:
            merged.append(list(current))
    # Since the last interval might be looping it needs to be checked if it intersects with the first one
    if len(merged) > 1 and intervals_intersect(merged[0], merged[-1], adapt_angles=True):
        merged[0] = interval_union(merged[0],merged.pop(-1))
    return merged


def hill_between(pixels, point_a, point_b):
    """Determine if a hill is between point a and point b

    Arguments:
        pixels {numpy.ndarray} -- pixel access
        point_a {tuple} -- current point
        point_b {tuple} -- target point

    Returns:
        bool -- if there is a hill between point a and point b
    """
    for point in bresenhamline(np.array([point_a]), np.array(point_b), -1):
        if get_pixel_save(pixels, point)[0] == settings.ABType.Hill.value:
            return True
    return False


def put_pixel_save(img, coords, color):
    """Save method for putting a color in image

    Arguments:
        img {Image} -- image
        coords {iterable} -- coordinates where to put the pixel
        color {tuple} -- each vale of the tuple represents the color value. None as value will leave the previous value
    """
    coords = tuple(int(c) for c in coords[:2])
    if not out_of_image(coords, True):
        if None in color:
            prev_color = img.getpixel(coords)
            color = tuple(c if c != None else p for c,
                          p in zip(color, prev_color))
        img.paste(Image.new("RGB", (settings.edge_length, settings.edge_length), color), coords)


def get_pixel_save(px, coords):
    """Save access to the pixels

    Arguments:
        px {PixelAccess} -- pixel access of the pillow library
        coords {tuple} -- coordinates

    Returns:
        tuple -- values at the given coordinate
    """
    if not isinstance(coords, tuple):
        coords = tuple(coords[:2])
    if not out_of_image(coords, True):
        return px[coords]
    return None

####
# From http://code.activestate.com/recipes/578112-bresenhams-line-algorithm-in-n-dimensions/
####


def _bresenhamline_nslope(slope):
    """
    Normalize slope for Bresenham's line algorithm.

    >>> s = np.array([[-2, -2, -2, 0]])
    >>> _bresenhamline_nslope(s)
    array([[-1., -1., -1.,  0.]])

    >>> s = np.array([[0, 0, 0, 0]])
    >>> _bresenhamline_nslope(s)
    array([[ 0.,  0.,  0.,  0.]])

    >>> s = np.array([[0, 0, 9, 0]])
    >>> _bresenhamline_nslope(s)
    array([[ 0.,  0.,  1.,  0.]])
    """
    scale = np.amax(np.abs(slope), axis=1).reshape(-1, 1)
    zeroslope = (scale == 0).all(1)
    scale[zeroslope] = np.ones(1)
    normalizedslope = np.array(slope, dtype=np.double) / scale
    normalizedslope[zeroslope] = np.zeros(slope[0].shape)
    return normalizedslope


def _bresenhamlines(start, end, max_iter):
    """
    Returns npts lines of length max_iter each. (npts x max_iter x dimension) 

    >>> s = np.array([[3, 1, 9, 0],[0, 0, 3, 0]])
    >>> _bresenhamlines(s, np.zeros(s.shape[1]), max_iter=-1)
    array([[[ 3,  1,  8,  0],
            [ 2,  1,  7,  0],
            [ 2,  1,  6,  0],
            [ 2,  1,  5,  0],
            [ 1,  0,  4,  0],
            [ 1,  0,  3,  0],
            [ 1,  0,  2,  0],
            [ 0,  0,  1,  0],
            [ 0,  0,  0,  0]],
    <BLANKLINE>
           [[ 0,  0,  2,  0],
            [ 0,  0,  1,  0],
            [ 0,  0,  0,  0],
            [ 0,  0, -1,  0],
            [ 0,  0, -2,  0],
            [ 0,  0, -3,  0],
            [ 0,  0, -4,  0],
            [ 0,  0, -5,  0],
            [ 0,  0, -6,  0]]])
    """
    if max_iter == -1:
        max_iter = np.amax(np.amax(np.abs(end - start), axis=1))
    npts, dim = start.shape
    nslope = _bresenhamline_nslope(end - start)

    # steps to iterate on
    stepseq = np.arange(1, max_iter + 1)
    stepmat = np.tile(stepseq, (dim, 1)).T

    # some hacks for broadcasting properly
    bline = start[:, np.newaxis, :] + nslope[:, np.newaxis, :] * stepmat

    # Approximate to nearest int
    return np.array(np.rint(bline), dtype=start.dtype)


def bresenhamline(start, end, max_iter=5):
    """
    Returns a list of points from (start, end] by ray tracing a line b/w the
    points.
    Parameters:
        start: An array of start points (number of points x dimension)
        end:   An end points (1 x dimension)
            or An array of end point corresponding to each start point
                (number of points x dimension)
        max_iter: Max points to traverse. if -1, maximum number of required
                  points are traversed

    Returns:
        linevox (n x dimension) A cumulative array of all points traversed by
        all the lines so far.

    >>> s = np.array([[3, 1, 9, 0],[0, 0, 3, 0]])
    >>> bresenhamline(s, np.zeros(s.shape[1]), max_iter=-1)
    array([[ 3,  1,  8,  0],
           [ 2,  1,  7,  0],
           [ 2,  1,  6,  0],
           [ 2,  1,  5,  0],
           [ 1,  0,  4,  0],
           [ 1,  0,  3,  0],
           [ 1,  0,  2,  0],
           [ 0,  0,  1,  0],
           [ 0,  0,  0,  0],
           [ 0,  0,  2,  0],
           [ 0,  0,  1,  0],
           [ 0,  0,  0,  0],
           [ 0,  0, -1,  0],
           [ 0,  0, -2,  0],
           [ 0,  0, -3,  0],
           [ 0,  0, -4,  0],
           [ 0,  0, -5,  0],
           [ 0,  0, -6,  0]])
    """
    # Return the points as a single array
    return _bresenhamlines(start, end, max_iter).reshape(-1, start.shape[-1])


def round_decimals_down(number: float, decimals: int = 2):
    """
    Returns a value rounded down to a specific number of decimal places.
    """
    if not isinstance(decimals, int):
        raise TypeError("decimal places must be an integer")
    elif decimals < 0:
        raise ValueError("decimal places has to be 0 or more")
    elif decimals == 0:
        return math.ceil(number)

    factor = 10 ** decimals
    return math.floor(number * factor) / factor


def round_decimals_up(number: float, decimals: int = 2):
    """
    Returns a value rounded up to a specific number of decimal places.
    """
    if not isinstance(decimals, int):
        raise TypeError("decimal places must be an integer")
    elif decimals < 0:
        raise ValueError("decimal places has to be 0 or more")
    elif decimals == 0:
        return math.ceil(number)

    factor = 10 ** decimals
    return math.ceil(number * factor) / factor

def floor_to_base(number: float, base: int = 1):
    """Get the floor multiple of base for number

    Arguments:
        number {float} -- given number

    Keyword Arguments:
        base {int} -- base corresponding to (default: {1})

    Returns:
        int -- a multiple of base
    """
    x = math.floor(number / base)
    return x * base
