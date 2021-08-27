from . import settings
import os
import planner.src.main.python.rebound as rebound
import numpy as np
import math
import logging
import time
import itertools
import random
import csv

fieldnames = ["file", "num_results", "time_first (s)", "time_last (s)", "time_max (s)", "edge_length", "consistency_check_length", "variance_after_bounce", "path_algorithm", "search_method", "tp", "fp", "tn", "fn", "first_iter", "last_iter"]

def run_simulations(files, bounce_variances, check_lengths, edge_lengths, path_algorithms, results_file, avg_per_file, start_interval, search_methods, random_select=False, bv_random_num=1, max_iterations=50000, bounce_variance_relative=True):
    print("--- Generating simulation data ---")
    # print(files, bounce_variances, check_lengths, edge_lengths, path_algorithms, results_file, avg_per_file, start_interval, search_methods, random_select, bv_random_num, max_iterations, bounce_variance_relative)
    # return
    with open(results_file,"w") as result_csv:
        writer = csv.DictWriter(result_csv,fieldnames=fieldnames)
        writer.writeheader()

    number_of_random_iterations = list(range(bv_random_num))
    if random_select:
        num_combinations = len(check_lengths) * len(edge_lengths) * len(path_algorithms) * len(number_of_random_iterations)
        # Run on average 100 simulations per file?
        probability_for_selection = avg_per_file / num_combinations

        print("Probability for selection is {} %".format(probability_for_selection*100))

    dist_available = rebound.between(start_interval[0],start_interval[1],bound=True)

    for file, edge_length, check_length, path_algorithm, search_method, i in itertools.product(files, edge_lengths, check_lengths, path_algorithms, search_methods, number_of_random_iterations):

        if random_select and random.random() > probability_for_selection:
            continue
        bounce_variance = random.random() * bounce_variances[1] + bounce_variances[0]
        print("\nrunning with parameters: file={}, edge_length={}, bounce_variance={}, check_length={}, path_algorithm={}, search_method={}".format(file, edge_length, bounce_variance, check_length, path_algorithm, search_method))
        start = time.time()
        iterator = rebound.run(file["img_file"], file["origin"], 0, start_interval, file["velocity"], file["target"], search_method=search_method, max_iter=max_iterations, log_level=logging.INFO, edge_length=edge_length, bounce_variance=bounce_variance, path_length=path_algorithm, consistency_check_length=check_length, bounce_variance_relative=bounce_variance_relative)
        result_list = []

        current = next(iterator, settings.generator_default)
        first_time = time.time() - start
        if current != settings.generator_default and isinstance(current, tuple):
            first_iter = current[2]
        else:
            first_iter = max_iterations
        last_iter = first_iter
        last_time = first_time
        while current != settings.generator_default:
            last_time = time.time() - start
            last_iter = current[2]
            result_list.append(current)
            current = next(iterator, settings.generator_default)

        end = time.time() - start
        
        true_positive, false_positive, true_negative, false_negative = errors_for_intervals(result_list, file["possible_intervals"], dist_available)

        print(true_positive, false_positive)
        print(true_negative, false_negative)

        with open(results_file, "a") as result_csv:
            writer = csv.DictWriter(result_csv,fieldnames=fieldnames)
            writer.writerow({
                fieldnames[0]: os.path.basename(file["img_file"]), 
                fieldnames[1]: len(result_list), 
                fieldnames[2]: first_time, 
                fieldnames[3]: last_time, 
                fieldnames[4]: end, 
                fieldnames[5]: edge_length, 
                fieldnames[6]: check_length, 
                fieldnames[7]: bounce_variance, 
                fieldnames[8]: path_algorithm, 
                fieldnames[9]: search_method, 
                fieldnames[10]: true_positive, 
                fieldnames[11]: false_positive, 
                fieldnames[12]: true_negative, 
                fieldnames[13]: false_negative, 
                fieldnames[14]: first_iter, 
                fieldnames[15]: last_iter, 
            })

def errors_for_intervals(actual, expected, max_size):
    if actual and not None in np.array(actual)[:,0]:

        intervals = rebound.merge_intervals(list(np.array(actual)[:,0]))
        print("result: {}, actual: {}".format(intervals, expected))
        overlapping = [0 for i in range(len(intervals))]
        not_overlapping = [0 for i in range(len(intervals))]
        
        for actual_interval in expected:
            for i,interval in enumerate(intervals):
                if rebound.intervals_intersect(actual_interval, interval, adapt_angles=True):
                    try:
                        intersection = rebound.interval_intersection(actual_interval, interval)

                        overlapping[i] += rebound.between(intersection[0],intersection[1], bound=True, ordered=True)
                    except ArithmeticError:
                        pass
        for i,interval in enumerate(intervals):
            if overlapping[i] == 0:
                not_overlapping[i] = rebound.between(interval[0],interval[1], bound=True, ordered=True)
            else:
                max_interval_size = rebound.between(interval[0],interval[1], bound=True, ordered=True)
                if overlapping[i] < max_interval_size:
                    not_overlapping[i] = max_interval_size - overlapping[i]

        overlapping_cum = {
            'negative': sum(not_overlapping),
            'positive': sum(overlapping)
        }
    else:
        overlapping_cum = {
            'negative': 0,
            'positive': 0
        }
    sum_possible = sum(map(lambda x: rebound.between(x[0], x[1], bound=True), expected))

    true_positive = overlapping_cum['positive'] / max_size
    false_positive = overlapping_cum['negative'] / max_size
    false_negative = (sum_possible - overlapping_cum['positive']) / max_size
    true_negative = 1 - true_positive - false_negative - false_positive

    return true_positive, false_positive, true_negative, false_negative

def main(args):
    run_simulations(
        files=[settings.files[int(i)-1] for i in set(args.files)],
        bounce_variances=[args.bv_min, args.bv_max],
        check_lengths=list(range(args.cl_min, args.cl_max+1, 1)), 
        edge_lengths=list(range(args.el_min, args.el_max+1, 1)), 
        path_algorithms=set(args.path_algorithms), 
        results_file=args.csv_file, 
        avg_per_file=args.random_avg_per_file,
        start_interval=[args.theta0_min, args.theta0_max],
        search_methods=set(args.search_methods),
        random_select=args.random,
        bv_random_num=args.bv_num,
        max_iterations=args.iterations,
        bounce_variance_relative=not args.bv_abs)
