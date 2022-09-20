import cv2 as cv
import matplotlib.pyplot as plt
import vision_watershed as w
import DiscreteCurve_2 as dc


def get_sift(object):
    # object is the cropped object in grayscale

    # sift
    sift = cv.SIFT_create(contrastThreshold=0.04, sigma=0.8)

    keypoints, descriptors = sift.detectAndCompute(object, None)

    return keypoints, descriptors


if __name__ == "__main__":
    # first example

    image_1 = cv.imread('/home/rocketqueen/sciencebirdsframework/screenshots/8.png')
    grey_image1 = cv.cvtColor(image_1, cv.COLOR_BGR2GRAY)
    watershed_obj_1 = w.Watershed(image_1)
    dst_1 = watershed_obj_1.get_dst()
    contours_1 = watershed_obj_1.get_objects()

    # Visualize the final image
    # cv.imshow('image_1', dst_1)
    # cv.waitKey(0)

    crop_obj_1 = dc.crop_obj(contours_1[10], grey_image1)
    # cv.imshow('cropped_1', crop_obj_1)
    # cv.waitKey(0)

    # the second example

    image_2 = cv.imread('/home/rocketqueen/sciencebirdsframework/screenshots/9.png')
    grey_image2 = cv.cvtColor(image_2, cv.COLOR_BGR2GRAY)
    watershed_obj_2 = w.Watershed(image_2)
    dst_2 = watershed_obj_2.get_dst()
    contours_2 = watershed_obj_2.get_objects()

    # Visualize the final image
    # cv.imshow('image_2', dst_2)
    # cv.waitKey(0)

    crop_obj_2 = dc.crop_obj(contours_2[14], grey_image2)
    # cv.imshow('cropped_2', crop_obj_2)
    # cv.waitKey(0)

    sift_1 = get_sift(crop_obj_1)
    sift_2 = get_sift(crop_obj_2)

    # print(str(len(sift_1[0])), str(len(sift_2[0])))

    bf = cv.BFMatcher(cv.NORM_L1, crossCheck=True)

    matches = bf.match(sift_1[1], sift_2[1])
    matches = sorted(matches, key=lambda x: x.distance)

    matches_img = cv.drawMatches(crop_obj_1, sift_1[0], crop_obj_2, sift_2[0], matches, crop_obj_2, flags=2)
    # cv.imshow('matches image', matches_img)
    # cv.waitKey(0)

    d2 = cv.matchShapes(crop_obj_1, crop_obj_2, cv.CONTOURS_MATCH_I2, 0)
    print(d2)
