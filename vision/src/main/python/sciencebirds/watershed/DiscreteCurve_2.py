from __future__ import print_function
import cv2 as cv
import numpy as np
import argparse
import random as rng
import matplotlib.pyplot as plt
import math
from ..polygon_operations import A_Star as a
import vision as v
from ..polygon_operations import Polygon as p
from . import vision_watershed as w

rng.seed(12345)

# Parameters
# in function get_vertices()
MIN_DST = 5


def crop_obj(contour, grey_image):
    x, y, w, h = cv.boundingRect(contour)

    min_x = x + 70
    min_y = y + 120
    max_x = min_x + w
    max_y = min_y + h

    cropped_obj = grey_image[min_y:max_y, min_x:max_x]
    return cropped_obj


def contour2latex(contour):
    contour_list = []
    for pair in contour:
        contour_list.append((pair[0][0] / 10, (274 - pair[0][1]) / 10))

    for pair in contour_list:
        print(pair, end=" --")


def calc_angle(vertex1, vertex2):
    Double_PI = 6.2831853071795865
    RAD2DEG = 57.2957795130823209

    if vertex1[1] == vertex2[1] and vertex1[0] == vertex2[0]:
        print("It is the same point")

    theta = math.atan2((vertex2[0] - vertex1[0]), (vertex2[1] - vertex1[1]))
    # if theta < 0.0:
    # theta += Double_PI
    return math.degrees(theta)


def calc_dist(vertex1, vertex2):
    euclidean_dist = math.sqrt(math.pow(
        (vertex1[1] - vertex2[1]), 2) + math.pow((vertex1[0] - vertex2[0]), 2))
    return euclidean_dist


def contour2poly(contour):
    vertices = []
    for j in range(0, len(contour)):
        vertices.append([contour[j][0][0] / 10, (274 - contour[j][0][1]) / 10])

    return p.Polygon(vertices)


# get the vertices of a contour, it simplifies the contour by deleting smaller edges and returns a polygon version of
# the vertices a polygon can be defined using p.Polygon(get_vertices(contour))
def get_vertices(contour):
    passed = False
    vertices = []
    x1 = contour[0][0][0]
    y1 = contour[0][0][1]
    start_vertex = (x1, y1)
    vertices.append(start_vertex)
    for j in range(1, len(contour)):
        # print("x2= " + str(contours[1][j][0][0]) + " y2= " + str(contours[1][j][0][1]))
        x2 = contour[j][0][0]
        y2 = contour[j][0][1]
        next_vertex = (x2, y2)
        if len(contour) > 3:
            if calc_dist(start_vertex, next_vertex) > MIN_DST:
                vertices.append(next_vertex)
                start_vertex = next_vertex
                passed = True
            else:
                passed = False
        else:
            vertices.append(next_vertex)
            start_vertex = next_vertex
        # print(calc_dist(start_vertex, next_vertex))

    if len(contour) > 3 and not passed:
        start_vertex = next_vertex
        next_vertex = (x1, y1)
        if calc_dist(start_vertex, next_vertex) > MIN_DST:
            vertices.append(start_vertex)

    poly_vertices = []
    for vertex in vertices:
        # image = cv.circle(dst, (vertex[0], vertex[1]), radius=1, color=(255, 255, 255), thickness=-2
        # print("vertices")
        # print("(" + str(vertex[0] / 10) + ", " + str((274 - vertex[1]) / 10) + ")", end=" --")
        poly_vertices.append([vertex[0] / 10, (274 - vertex[1]) / 10])

    return poly_vertices


if __name__ == "__main__":

    # crop object and extract sift keys
    # crop_obj = gray[32:103, 369:439]
    # sift = cv.SIFT_create()
    # kp = sift.detect(crop_obj, None)
    # img = cv.drawKeypoints(crop_obj, kp, crop_obj)
    # cv.imshow('Cropped Object', img)
    # cv.waitKey(0)

    # calculate the histogram
    # hist = cv.calcHist([gray], [0], None, [256], [0, 256])
    # plt.plot(hist)
    # plt.show()

    image1 = cv.imread(
        '/home/rocketqueen/sciencebirdsframework/screenshots/8.png')
    watershed_obj = w.Watershed(image1)
    dst = watershed_obj.get_dst()
    contours = watershed_obj.get_objects()

    cv.imshow('dst', dst)
    cv.waitKey(0)

    # print all contours in latex form
    for i, c in enumerate(contours):
        print(i)
        contour2latex(c)

    polygon_vertices = get_vertices(contours[48])
    print(contours[48])
    polygon = p.Polygon(polygon_vertices)
    print(polygon.vertices)
    results = a.split_A_star(polygon)
    print("RESULTS")
    for r in results:
        print(r.vertices)
        lines = r.get_lines()
        for line in lines:
            image = cv.line(dst, (int(line[0][0] * 10), int(274 - line[0][1] * 10)),
                            (int(line[1][0] * 10), int(274 -
                                                       line[1][1] * 10)), (255, 255, 255),
                            thickness=1)

    scale_percent = 200
    # calculate the 50 percent of original dimensions
    width = int(image.shape[1] * scale_percent / 100)
    height = int(image.shape[0] * scale_percent / 100)
    dsize = (width, height)
    # resize image
    img = cv.resize(image, dsize)
    cv.imshow('Final Result', img)
    cv.waitKey(0)

    croppy = crop_obj(contours[21], image1)
    scale_percent = 200
    # calculate the 50 percent of original dimensions
    width = int(croppy.shape[1] * scale_percent / 100)
    height = int(croppy.shape[0] * scale_percent / 100)
    dsize = (width, height)
    # resize image
    croppy = cv.resize(croppy, dsize)
    cv.imshow('Final Result', croppy)
    cv.waitKey(0)
