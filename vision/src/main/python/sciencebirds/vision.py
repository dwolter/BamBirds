from . import watershed as w
from .polygon_operations import A_Star as a
from .polygon_operations import Polygon as p


import cv2 as cv
import numpy as np
import argparse
import random as rng
import matplotlib.pyplot as plt
import math
from .watershed import DiscreteCurve_2 as dc


def calc_dist(vertex1, vertex2):
    euclidean_dist = math.sqrt(math.pow(
        (vertex1[1] - vertex2[1]), 2) + math.pow((vertex1[0] - vertex2[0]), 2))
    return euclidean_dist


def main(args):
    line_thickness = 1
    MIN_AREA = args.min_area

    image = cv.imread(args.image)

    watershed_obj = w.Watershed(image)
    watershed_img = watershed_obj.get_dst()
    contours = watershed_obj.get_objects()

    for i, contour in enumerate(contours, 0):
        if cv.contourArea(contour) > MIN_AREA:
            vertices = dc.get_vertices(contour)
            polygon = p.Polygon(vertices)
            results = a.split_A_star(polygon, debug=args.debug)
            if results is not None:
                for r in results:
                    lines = r.get_lines()
                    for line in lines:
                        cv.line(watershed_img, (int(line[0][0] * 10), int(274 - line[0][1] * 10)),
                                (int(line[1][0] * 10), int(274 -
                                                           line[1][1] * 10)), (255, 255, 255),
                                thickness=line_thickness)

    # TODO: Convert watershed_obj to json representation of a scene

    scale_percent = args.scale_percent
    width = int(watershed_img.shape[1] * scale_percent / 100)
    height = int(watershed_img.shape[0] * scale_percent / 100)
    dsize = (width, height)

    # resize image
    img = cv.resize(watershed_img, dsize)
    if args.debug:
        cv.imshow('Final Result', img)
        cv.waitKey(0)
    cv.imwrite(args.image.replace('.png', '_watershed.png'), img)
    print("done")


def create_argparser(parser: argparse.ArgumentParser = None) -> argparse.ArgumentParser:
    if parser == None:
        parser = argparse.ArgumentParser()
    parser.add_argument('image', type=str,
                        help="The image to analyze")
    parser.add_argument(
        '-o', '--output', type=str, help="Where to output the result. If omitted, result json is printed to stdout")
    parser.add_argument('--scale-percent', type=int,
                        default=200, help="Scaling percentage")
    parser.add_argument('--min-area', type=int, default=200,
                        help="Minimum area for objects")
    parser.add_argument('--debug', action='store_true', help="Enable debug mode")
    parser.set_defaults(func=main)


if __name__ == "__main__":
    parser = create_argparser()

    args = parser.parse_args()
    main(args)
