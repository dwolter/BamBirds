from PIL import Image
import os
import numpy as np
import ntpath

ground_truth_file = "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/ground_truth_130721_standard_framerate.csv"
result_file = "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/ground_truth_130721_standard_framerate_dependence_result.csv"
compare_file = "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/ground_truth_130721_standard_framerate_comparison.csv"
area_of_motion_result_file = "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/area_of_motion_result_file.csv"
params_erode = ""


def open_files():
        
    with open(ground_truth_file) as gf, open(result_file) as rf:
        ground_truth_lines = gf.readlines()
        result_lines = rf.readlines()
        return ground_truth_lines, result_lines

def compare_files():
    FP,FN,TP,TN = 0,0,0,0
    check_for_existing_compare_file()
    ground_truth_lines, result_lines = open_files()

    for ground_truth_line in ground_truth_lines:
        split_ground_truth_line  = ground_truth_line.split(',')
        for result_line in result_lines:
            if (split_ground_truth_line[0] in result_line and split_ground_truth_line[1] in result_line):
                FP,FN,TP,TN = results_match(ground_truth_line,result_line,FP,FN,TP,TN)
    print_compare_results("FP: "+str(FP)+" FN "+str(FN)+" TP "+str(TP)+" TN "+str(TN)+" params "+params_erode)

def results_match(ground_truth_line,result_line,FP,FN,TP,TN):
    
    ground_truth_line.strip("//")
    if ("True" in ground_truth_line and "True" in result_line):
        TP +=1
        print_compare_results("True Positive, gt: "+ground_truth_line+" rs: "+result_line)
    elif ("False" in ground_truth_line and "False" in result_line):
        TN +=1
        print_compare_results("True Negative, gt: "+ground_truth_line+" rs: "+result_line)
    elif ("True" in ground_truth_line and "False" in result_line):
        FN += 1
        print_compare_results("False Negative, gt: "+ground_truth_line+" rs: "+result_line)
    elif ("False" in ground_truth_line and "True" in result_line):
        FP += 1
        print_compare_results("False Positive, gt: "+ground_truth_line+" rs: "+result_line)
    return FP,FN,TP,TN

def check_for_existing_compare_file():
    if os.path.isfile(compare_file):
        with open(compare_file,'w') as cf:
            cf.truncate() 
            cf.close()
    else:
        open(compare_file, 'a').close()

def print_compare_results(line_to_print):
    with open (compare_file, 'a') as comp_file:
        comp_file.write(line_to_print)

def check_area_of_motion_match(eroded_pics_paths, drawn_pics_path, result_path):
    same_pixels_positions = []
    different_pixel_positions = []
    black_in_eroded_pics_positions = []
    different_pixels = 0
    same_pixels_black = 0
    black_in_eroded_pics = 0

    for eroded_pics_path in eroded_pics_paths:
        img1_file = Image.open(eroded_pics_path)
        img2_file = Image.open(drawn_pics_path)
        if not os.path.isfile(result_path):
            a = np.full((img1_file.size[1], img1_file.size[0], 3), 255, dtype=np.uint8)
            image = Image.fromarray(a, "RGB")
            image.save(result_path, "PNG")
            

        result_file = Image.open(result_path)

        pic_matrix_1 = img1_file.load()
        pic_matrix_2 = img2_file.load()
        result_matrix = result_file.load()

        for x in range(img1_file.size[0]):
            for y in range(img1_file.size[1]):
                pic_matrix_1[x,y], pic_matrix_2[x,y] = adjust_values_to_next_binary(pic_matrix_1[x,y], pic_matrix_2[x,y])
                
                if pic_matrix_1[x,y][:3] == (0,0,0) and not [x,y] in black_in_eroded_pics_positions:
                    black_in_eroded_pics += 1
                    black_in_eroded_pics_positions.append([x,y])

                if pic_matrix_1[x,y][:3] != pic_matrix_2[x,y][:3]: # The two compared images are different
                    if not [x,y] in different_pixel_positions and [x,y] not in same_pixels_positions: 
                        # The position is not already counted as different in two images, and is not overlapping in two images
                        different_pixels += 1
                        different_pixel_positions.append([x,y])

                elif pic_matrix_1[x,y][:3] == pic_matrix_2[x,y][:3] and pic_matrix_1[x,y][:3] != (255,255,255) and pic_matrix_2[x,y][:3] != (255,255,255):
                    # Two positions correspond
                    if not [x,y] in same_pixels_positions: # Not already marked as overlapping
                        same_pixels_black += 1
                        result_matrix[x,y] = (0,0,0) # Paint black in result image
                        same_pixels_positions.append([x,y])
                        if [x,y] in different_pixel_positions: # If marked as non overlapping, mark as overlapping now
                            different_pixels -= 1
                            different_pixel_positions.remove([x,y])
    
    result_file.save(result_path)

    total_black_area = different_pixels + same_pixels_black

    drawn_in_eroded_to_total_black_area = same_pixels_black / total_black_area

    marked_by_gimp_to_drawn_in_eroded = same_pixels_black / black_in_eroded_pics

    print("drawn_in_eroded_to_total_black_area: "+str(drawn_in_eroded_to_total_black_area))

    print("marked_by_gimp_to_drawn_in_eroded: "+str(marked_by_gimp_to_drawn_in_eroded))

    level_string_list = eroded_pics_paths[0].split("/")
    level_string = [s for s in level_string_list if "level" in s][0]

    print_results_to_csv(different_pixels,same_pixels_black,total_black_area, level_string, drawn_pics_path, drawn_in_eroded_to_total_black_area,marked_by_gimp_to_drawn_in_eroded)
                
    
    

def print_results_to_csv(different_pixels,same_pixels_black,total_black_area, img1_path, img2_path, drawn_in_eroded_to_total_black_area,marked_by_gimp_to_drawn_in_eroded):
    with open(area_of_motion_result_file, "a") as myfile:
        myfile.write(img1_path+","+ntpath.basename(img2_path)+","+str(different_pixels)+","+str(same_pixels_black)+","+str(total_black_area)+",drawn_in_eroded_to_total_black_area: "+str(drawn_in_eroded_to_total_black_area)+",marked_by_gimp_to_drawn_in_eroded: "+str(marked_by_gimp_to_drawn_in_eroded)+"\n")


def adjust_values_to_next_binary(pic_matrix_1_values, pic_matrix_2_values):

    if all (i < 125 for i in pic_matrix_1_values ):
        if len(pic_matrix_1_values) == 3:
            pic_matrix_1_values = (0,0,0)
        elif len(pic_matrix_1_values) == 4:
            pic_matrix_1_values = (0,0,0,0)

    elif all ((255-i) < 124 for i in pic_matrix_1_values):
        if len(pic_matrix_1_values) == 3:
            pic_matrix_1_values = (255,255,255)
        elif len(pic_matrix_1_values) == 4:
            pic_matrix_1_values = (255,255,255,255)

    if all (i < 125 for i in pic_matrix_2_values ):
        if len(pic_matrix_1_values) == 3:
            pic_matrix_1_values = (0,0,0)
        elif len(pic_matrix_1_values) == 4:
            pic_matrix_1_values = (0,0,0,0)

    elif all ((255-i) < 124 for i in pic_matrix_2_values):
        if len(pic_matrix_1_values) == 3:
            pic_matrix_1_values = (255,255,255)
        elif len(pic_matrix_1_values) == 4:
            pic_matrix_1_values = (255,255,255,255)
    

    return pic_matrix_1_values, pic_matrix_2_values
            

    

if __name__ == "__main__":
    compare_files()
    """
    eroded_pics_paths = ["/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/level_2/2-1_dil_size_2_dil_shape_2.jpg","/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/level_2/2-2_dil_size_2_dil_shape_2.jpg",
    "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/level_2/2-3_dil_size_2_dil_shape_2.jpg"]
    check_area_of_motion_match(
        eroded_pics_paths, 
        "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/level_2/gimp/level2_layered_screenshots_w_affected_area.png", 
        "/home/nomis/Documents/AIBIRDS/Doc/analysis/pixel_diff/level_2/gimp/result.png")
        """