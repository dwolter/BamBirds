import pickle

from sklearn.cluster import KMeans
import cv2 as cv
import vision_watershed as w
import DiscreteCurve_2 as dc
import SIFT as s
import numpy as np
import matplotlib.pylab as plab
import matplotlib.pyplot as plt

image = cv.imread('/home/rocketqueen/sciencebirdsframework/screenshots/8.png')
gray_image = cv.cvtColor(image, cv.COLOR_BGR2GRAY)
watershed_obj = w.Watershed(image)
dst = watershed_obj.get_dst()
contours = watershed_obj.get_objects()

# cv.imshow('cropped_1', gray_image)
# cv.waitKey(0)

descriptor_list = []

for contour in contours:
    obj = dc.crop_obj(contour, gray_image)
    obj_descriptors = np.asarray(s.get_sift(obj)[1])
    if not np.any(obj_descriptors == None):
        descriptor_list.append(obj_descriptors)

descriptor_list = np.vstack(descriptor_list)

# setting random seed to an arbitary int number so we will get the same results every time it runs

k_means = KMeans(n_clusters=18, random_state=42)
k_means.fit(descriptor_list)

obj = dc.crop_obj(contours[13], gray_image)
obj_descriptors = np.asarray(s.get_sift(obj)[1])
if np.any(obj_descriptors == None):
    print("none")
else:
    vocab = k_means.predict(obj_descriptors)
    vocab_string_1 = ""
    for v in vocab:
        if v / 10 < 1:
            v = "0" + str(v)
        vocab_string_1 = vocab_string_1 + " " + str(v)
    # print(vocab_string)
    hist, bin_edges = np.histogram(vocab, bins=range(18))
    # print(list(hist))

obj = dc.crop_obj(contours[10], gray_image)
obj_descriptors = np.asarray(s.get_sift(obj)[1])

if np.any(obj_descriptors == None):
    print("none")
else:
    vocab = k_means.predict(obj_descriptors)
    vocab_string_2 = ""
    for v in vocab:
        if v / 10 < 1:
            v = "0" + str(v)
        vocab_string_2 = vocab_string_2 + " " + str(v)
    # print(vocab_string_2)
    hist, bin_edges = np.histogram(vocab, bins=range(18))
    # print(list(hist))


# cv.imshow('cropped', obj)
# cv.waitKey(0)


def get_vocab_string():
    return vocab_string_1, vocab_string_2


if __name__ == "__main__":

    descriptor_list = np.loadtxt('/home/rocketqueen/uni bamberg/sose 2020/AI birds/results_100/descriptors', dtype=float)

    # k_means = KMeans(n_clusters=80, random_state=42)
    # k_means.fit(descriptor_list)

    # save the model to disk
    filename = '/home/rocketqueen/uni bamberg/sose 2020/AI birds/results_100/clusters.sav'
    # pickle.dump(k_means, open(filename, 'wb'))

    # print(k_means.labels_)

    # load the model from disk
    k_means = pickle.load(open(filename, 'rb'))

    obj_descriptors2 = np.loadtxt('/home/rocketqueen/uni bamberg/sose 2020/AI birds/results_100/descriptors_70')
    obj_descriptors2 = obj_descriptors2.astype(float)

    print(k_means.predict(obj_descriptors2))

    obj_descriptors3 = np.loadtxt('/home/rocketqueen/uni bamberg/sose 2020/AI birds/results_100/descriptors_99')
    obj_descriptors3 = obj_descriptors3.astype(float)

    print(k_means.predict(obj_descriptors3))

    obj_descriptors3 = np.loadtxt('/home/rocketqueen/uni bamberg/sose 2020/AI birds/results_100/descriptors_73')
    obj_descriptors3 = obj_descriptors3.astype(float)

    print(k_means.predict(obj_descriptors3))
