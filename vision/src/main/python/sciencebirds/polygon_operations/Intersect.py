# This code is implemented using this tutorial:
# https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/

class Point:
    def __init__(self, a, b):
        self.a = a
        self.b = b


def on_segment(x, y, z):
    if ((y.a <= max(x.a, z.a)) and (y.a >= min(x.a, z.a)) and
            (y.b <= max(x.b, z.b)) and (y.b >= min(x.b, z.b))):
        return True
    return False


def orient(x, y, z):
    val = (float(y.b - x.b) * (z.a - y.a)) - (float(y.a - x.a) * (z.b - y.b))
    if val > 0:

        return 1
    elif val < 0:

        return 2
    else:

        return 0


def do_intersect(p1, q1, p2, q2):
    o1 = orient(p1, q1, p2)
    o2 = orient(p1, q1, q2)
    o3 = orient(p2, q2, p1)
    o4 = orient(p2, q2, q1)

    if (o1 != o2) and (o3 != o4):
        return True

    if (o1 == 0) and on_segment(p1, p2, q1):
        return True

    if (o2 == 0) and on_segment(p1, q2, q1):
        return True

    if (o3 == 0) and on_segment(p2, p1, q2):
        return True

    if (o4 == 0) and on_segment(p2, q1, q2):
        return True

    return False
