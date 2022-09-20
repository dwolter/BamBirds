import os
import cv2 as cv
import ntpath
import numpy as np
from PIL import Image
import testscript

dilate_folder = "/home/nomis/Documents/AIBIRDS/Doc/dil_results/"
dilate_path1 = ""
dilate_path2 = ""

def check_for_existing_results_file(res_file_path):
    if os.path.isfile(res_file_path):
        with open(res_file_path,'w') as cf:
            cf.truncate() 
            cf.close()

def dilation(image_path, dilation_shape, dilation_size):
    src = cv.imread(cv.samples.findFile(image_path))
    if src is None:
        print('Could not open or find the image: ', image_path)

    element = cv.getStructuringElement(dilation_shape, (2 * dilation_size + 1, 2 * dilation_size + 1),
                                       (dilation_size, dilation_size))

    dilation_dst = cv.dilate(src, element)
    dilate_path = dilate_folder+ntpath.basename(image_path)[:3] +"_dil_size_"+str(dilation_size)+"_dil_shape_"+str(dilation_shape)+".jpg"
    cv.imwrite(dilate_path,dilation_dst)
    return dilate_path

""" def overlap():
    img1 = cv.imread("/home/nomis/Documents/AIBIRDS/Doc/analysis/erode/thresh_0_dil_2/5-1_thresh_0_8e9f5451-4b81-43d3-985e-407699dde5c1_2021-05-27T21:01:10.529.jpg_dil_size_2.jpg",0)
    img2 = cv.imread("/home/nomis/Documents/AIBIRDS/Doc/analysis/erode/thresh_0_dil_2/5-2_thresh_0_df2ed4e2-a2da-4697-93dc-ce80bdc277d7_2021-05-27T21:01:30.332.jpg_dil_size_2.jpg",0)

    img_bwa = cv.bitwise_and(img1,img2)
    img_bwo = cv.bitwise_or(img1,img2)
    img_bwx = cv.bitwise_xor(img1,img2)

    cv.imwrite("/home/nomis/Documents/AIBIRDS/Doc/analysis/erode/thresh_0_dil_2/bwo.jpg", img_bwo) """


def area_dependence(img1_path,img2_path, threshold):
    img1_file = Image.open(img1_path)
    img2_file = Image.open(img2_path)

    pic_matrix_1 = img1_file.load()
    pic_matrix_2 = img2_file.load()

    different_pixels = 0
    same_pixels_black = 0

    for x in range(img1_file.size[0]):
        for y in range(img1_file.size[1]):
            pic_matrix_1[x,y], pic_matrix_2[x,y] = adjust_values_to_next_binary(pic_matrix_1[x,y], pic_matrix_2[x,y])
            if pic_matrix_1[x,y] != pic_matrix_2[x,y]:
                different_pixels += 1
            elif pic_matrix_1[x,y] == pic_matrix_2[x,y] and pic_matrix_1[x,y] != (255,255,255) and pic_matrix_2[x,y] != (255,255,255):
                same_pixels_black += 1
            """ if any((i != 255 and i != 0) for i in pic_matrix_1[x,y]):
                print (pic_matrix_1[x,y])
                similar_values_counter += 1 """
        
    total_black_area = different_pixels + same_pixels_black

    print (different_pixels)
    print (same_pixels_black)
    print (total_black_area)

    return same_pixels_black/total_black_area  > threshold, different_pixels, same_pixels_black, total_black_area

def adjust_values_to_next_binary(pic_matrix_1_values, pic_matrix_2_values):
    if all (i < 125 for i in pic_matrix_1_values ):
        pic_matrix_1_values = (0,0,0)

    elif all ((255-i) < 124 for i in pic_matrix_1_values):
        pic_matrix_1_values = (255,255,255)

    if all (i < 125 for i in pic_matrix_2_values ):
        pic_matrix_2_values = (0,0,0)

    elif all ((255-i) < 124 for i in pic_matrix_2_values):
        pic_matrix_2_values = (255,255,255)
    
    return pic_matrix_1_values, pic_matrix_2_values
 
def print_results_to_file(image1_path, image2_path, different_pixels,same_pixels_black,total_black_area,area_dependence_result,threshold,result_file_path):
    with open(result_file_path, "a") as myfile:
        myfile.write(ntpath.basename(image1_path)+","+ntpath.basename(image2_path)+","+str(different_pixels)+","+str(same_pixels_black)+","+str(total_black_area)+","+str(threshold)+","+str(area_dependence_result)+"\n")

def read_gt_and_launch(ground_truth_file, result_file_path, dependence_threshold, dilation_shape, dilation_size):
    with open(ground_truth_file) as gf:
        ground_truth_lines = gf.readlines()
        for ground_truth_line in ground_truth_lines:
            split_ground_truth_line  = ground_truth_line.split(',')
            image1_path = find_full_path(split_ground_truth_line[0])
            image2_path = find_full_path(split_ground_truth_line[1])

            dilate_path_1 = dilation(image1_path, dilation_shape, dilation_size)
            dilate_path_2 = dilation(image2_path, dilation_shape, dilation_size)

            area_dependence_result, different_pixels, same_pixels_black, total_black_area = area_dependence(dilate_path_1,dilate_path_2,dependence_threshold)

            print(area_dependence_result)

            print_results_to_file(image1_path, image2_path, different_pixels,same_pixels_black,total_black_area,area_dependence_result,dependence_threshold,result_file_path)

def find_full_path(filename):
    for root, dirs, files in os.walk("/home/nomis/Documents/AIBIRDS/Doc/"):
        for name in files:
            if name == filename:
                return os.path.abspath(os.path.join(root, name))

if __name__ == "__main__": # Set parameters here!
    dilation_size = 2
    dilation_shape = 2 # { "Rectangle", "Cross", "Ellipse" } = 0,1,2?
    dependence_threshold = 0.02
    burst_rate ="high"

    testscript.params_erode = ("dil_size_"+str(dilation_size)+"_dil_shape_"+str(dilation_shape)+"_dependence_thresh_"+str(dependence_threshold)
    +"_burstrate_"+burst_rate)

    gt_file_path = "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/no_wait_quickshot/ground_truth_no_wait_quickshot.csv"
    testscript.ground_truth_file = gt_file_path
    res_file_path ="/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/no_wait_quickshot/dependence_result_"+testscript.params_erode+".csv"
    testscript.result_file = res_file_path
    testscript.compare_file = "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/no_wait_quickshot/compare_file"+testscript.params_erode+".csv"
    check_for_existing_results_file(res_file_path)
    read_gt_and_launch(gt_file_path,res_file_path, dependence_threshold, dilation_shape, dilation_size)
    testscript.compare_files()