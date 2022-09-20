
from __future__ import print_function
import cv2 as cv
import numpy as np
import argparse
import random as rng
import matplotlib.pyplot as plt

rng.seed(12345)

if __name__ == "__main__":

    image = cv.imread('/home/rocketqueen/sciencebirdsframework/screenshots/8.png')

    # image = cv.imread('/home/rocketqueen/Bilder/water_coins.jpg')

    crop_img = image[120:394, 70:700]

    # scale_percent = 125
    # calculate the 50 percent of original dimensions
    # width = int(crop_img.shape[1] * scale_percent / 100)
    # height = int(crop_img.shape[0] * scale_percent / 100)
    # dsize
    # dsize = (width, height)
    # resize image
    # img = cv.resize(crop_img, dsize)
    cv.imshow('input image', crop_img)
    cv.waitKey(0)

    gray = cv.cvtColor(crop_img, cv.COLOR_BGR2GRAY)
    cv.imshow('grey', gray)
    cv.waitKey(0)

    hist = cv.calcHist([gray], [0], None, [256], [0, 256])
    plt.plot(hist)
    plt.show()

    ret, thresh = cv.threshold(gray, 222, 255, cv.THRESH_BINARY_INV)
    cv.imshow('binary image', thresh)
    cv.waitKey(0)
    print(gray.shape)

    # noise removal
    kernel = cv.getStructuringElement(cv.MORPH_CROSS, (3, 3))

    opening = cv.morphologyEx(thresh, cv.MORPH_OPEN, kernel,
                              iterations=1)
    cv.imshow('opening', opening)
    cv.waitKey(0)

    # sure background area
    kernel_close = cv.getStructuringElement(cv.MORPH_CROSS, (2, 2))
    sure_bg = cv.dilate(opening, kernel_close, iterations=2)
    # Finding sure foreground area
    dist_transform = cv.distanceTransform(opening, cv.DIST_L2, 5)
    cv.normalize(dist_transform, dist_transform, 0
                 , 1.0, cv.NORM_MINMAX)
    cv.imshow('dist transform', dist_transform)
    cv.waitKey(0)

    ret, sure_fg = cv.threshold(dist_transform, 0.1
                                * dist_transform.max(), 255, 0)

    # Finding unknown region
    sure_fg = np.uint8(sure_fg)
    unknown = cv.subtract(sure_bg, sure_fg)

    cv.imshow('foreground', sure_fg)
    cv.waitKey(0)

    cv.imshow('background', sure_bg)
    cv.waitKey(0)

    sure_fg_u8 = sure_fg.astype('uint8')
    contours, _ = cv.findContours(sure_fg_u8, cv.RETR_EXTERNAL
                                  , cv.CHAIN_APPROX_SIMPLE)

    print(str(len(contours)))

    markers = np.zeros(sure_fg.shape, dtype=np.int32)
    for i in range(len(contours)):
        cv.drawContours(markers, contours, i, (i + 1), -1)
    # Draw the background marker
    cv.circle(markers, (5, 5), 3, (255, 255, 255), -1)

    cv.watershed(crop_img, markers)

    # Marker labelling
    # ret, markers = cv.connectedComponents(sure_fg)

    # Add one to all labels so that sure background is not 0, but 1
    # markers = markers + 1
    # Now, mark the region of unknown with zero
    # markers[unknown == 255] = 0

    # markers = cv.watershed(crop_img, markers)

    # crop_img[markers == -1] = [255, 0, 0]
    # cv.imshow('output', crop_img)
    # cv.waitKey(0)

    # drawing result with different colors

    colors = []
    for contour in contours:
        colors.append((rng.randint(0, 256), rng.randint(0, 256), rng.randint(0, 256)))

    # Create the result image
    dst = np.zeros((markers.shape[0], markers.shape[1], 3), dtype=np.uint8)
    # Fill labeled objects with random colors
    for i in range(markers.shape[0]):
        for j in range(markers.shape[1]):
            index = markers[i, j]
            if 0 < index <= len(contours):
                dst[i, j, :] = colors[index - 1]
    # Visualize the final image
    cv.imshow('Final Result', dst)
    cv.waitKey(0)
