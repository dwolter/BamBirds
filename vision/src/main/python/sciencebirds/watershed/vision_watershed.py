from __future__ import print_function
import cv2 as cv
import numpy as np
import argparse
import random as rng

rng.seed(12345)


class Watershed:
    def __init__(self, image):

        self.crop_img = image[120:394, 70:700]

        # scale_percent = 125
        # calculate the 50 percent of original dimensions
        # width = int(crop_img.shape[1] * scale_percent / 100)
        # height = int(crop_img.shape[0] * scale_percent / 100)
        # dsize
        # dsize = (width, height)
        # resize image
        # img = cv.resize(crop_img, dsize)

        self.gray = cv.cvtColor(self.crop_img, cv.COLOR_BGR2GRAY)

        ret, thresh = cv.threshold(self.gray, 222, 255, cv.THRESH_BINARY_INV)

        # noise removal
        kernel = cv.getStructuringElement(cv.MORPH_CROSS, (3, 3))

        opening = cv.morphologyEx(thresh, cv.MORPH_OPEN, kernel,
                                  iterations=1)

        # sure background area
        kernel_close = cv.getStructuringElement(cv.MORPH_CROSS, (2, 2))
        sure_bg = cv.dilate(opening, kernel_close, iterations=2)
        # Finding sure foreground area
        dist_transform = cv.distanceTransform(opening, cv.DIST_L2, 5)
        cv.normalize(dist_transform, dist_transform, 0
                     , 1.0, cv.NORM_MINMAX)

        ret, sure_fg = cv.threshold(dist_transform, 0.1
                                    * dist_transform.max(), 255, 0)

        # Finding unknown region
        sure_fg = np.uint8(sure_fg)
        unknown = cv.subtract(sure_bg, sure_fg)

        sure_fg_u8 = sure_fg.astype('uint8')
        self.contours, _ = cv.findContours(sure_fg_u8, cv.RETR_TREE
                                      , cv.CHAIN_APPROX_SIMPLE)

        # Marker labelling
        ret, markers = cv.connectedComponents(sure_fg)

        # Add one to all labels so that sure background is not 0, but 1
        markers = markers + 1
        # Now, mark the region of unknown with zero
        markers[unknown == 255] = 0

        markers = cv.watershed(self.crop_img, markers)

        # crop_img[markers == -1] = [255, 0, 0]
        # cv.imshow('output', crop_img)
        # cv.waitKey(0)

        # drawing result with different colors

        colors = []
        for contour in self.contours:
            colors.append((rng.randint(0, 256), rng.randint(0, 256), rng.randint(0, 256)))

        # Create the result image
        dst = np.zeros((markers.shape[0], markers.shape[1], 3), dtype=np.uint8)
        # Fill labeled objects with random colors
        for i in range(markers.shape[0]):
            for j in range(markers.shape[1]):
                index = markers[i, j]
                if 0 < index <= len(self.contours):
                    dst[i, j, :] = colors[index - 1]

        self.dst = dst

    def get_objects_num(self):
        return len(self.contours)

    def get_objects(self):
        return self.contours

    def get_dst(self):
        return self.dst
