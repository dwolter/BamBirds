import cv2 as cv2
import numpy as np


def checkRep(b, g, r):
    if (b == g).all() and (b == r).all():
        print("Gray")
    elif np.all(b == 0) or np.all(r == 0) or np.all(g == 0):
        print('One Color')
    else:
        print("Colored")


if __name__ == '__main__':
    img = cv2.imread('/home/rocketqueen/sciencebirdsframework/screenshots/92.png')
    blue, green, red = img[:, :, 0], img[:, :, 1], img[:, :, 2]
    checkRep(blue, green, red)

    img[:, :, 0] = img[:, :, 2]
    img[:, :, 1] = img[:, :, 2]

    checkRep(blue, green, red)
    cv2.imshow('gray image', img)
    cv2.waitKey(0)

    img[:, :, 0] = 0
    img[:, :, 1] = 0
    checkRep(blue, green, red)
    cv2.imshow('red image', img)
    cv2.waitKey(0)
