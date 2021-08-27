import unittest
import os
import numpy as np
import math
import logging
import time
import itertools
import random
import planner.src.main.python.rebound as rebound
from . import settings

class TestRunning(unittest.TestCase):

    @unittest.skip("Outdated")
    def test_running_df(self):
        print("--- test_running_df ---")
        pt = (520, 368)
        target = (522, 376)
        rg = [
            [
                5,
                50
            ],
            [
                90,
                50
            ]
        ]

        self.assertNotEqual(rebound.run(
            settings.scene_files[10], pt, 1, rg, search_method="df", max_iter=50000, log_level=logging.INFO), None, "Expected the output not to be empty")

    @unittest.skip("Outdated")
    def test_running_bf(self):
        print("--- test_running_bf ---")
        pt = (520, 368)
        target = (522, 376)
        rg = [
            [
                5,
                50
            ],
            [
                90,
                50
            ]
        ]
        self.fail("Disabled")
        self.assertNotEqual(rebound.run(
            settings.scene_files[10], pt, 1, rg, search_method="bf", max_iter=10000, log_level=logging.INFO), None, "Expected the output not to be empty")

    @unittest.skip("Outdated")
    def test_running_start_df(self):
        print("--- test_running_start_df ---")
        pt = (306, 316)
        target = (522, 376)
        rg = [271, 89]
        velocity = 75
        self.fail("Disabled")
        self.assertNotEqual(rebound.run(
            settings.scene_files[10], pt, 0, rg, velocity, search_method="df", max_iter=10000, log_level=logging.INFO), None, "Expected the output not to be empty")

    @unittest.skip("Outdated")
    def test_running_start_df_second_image(self):
        print("--- test_running_start_df_second_image ---")
        pt = (260, 316)
        target = (571, 331)
        rg = [0, 89]
        velocity = 50
        self.fail("Disabled")
        self.assertNotEqual(rebound.run(
            settings.scene_files[11], pt, 0, rg, velocity, target, search_method="df", max_iter=10000, log_level=logging.INFO), None, "Expected the output not to be empty")

    @unittest.skip("Outdated")
    def test_running_start_bf(self):
        print("--- test_running_start_bf ---")
        pt = (306, 316)
        target = (522, 376)
        rg = [45, 89]
        velocity = 75
        self.fail("Disabled")
        self.assertNotEqual(rebound.run(
            settings.scene_files[10], pt, 0, rg, velocity, target, search_method="bf", max_iter=10000, log_level=logging.INFO), None, "Expected the output not to be empty")

    @unittest.skip("Outdated")
    def test_running_start_heuristic(self):
        print("--- test_running_start_heuristic ---")
        pt = (297, 331)
        target = (522, 376)
        rg = [271, 89]
        velocity = 75
        self.fail("Disabled")
        result = [i for i,j in zip(rebound.run(
            settings.scene_files[10], pt, 0, rg, velocity, target, search_method="heuristic", max_iter=50000, log_level=logging.INFO, bounce_variance=0, edge_length=10,path_length='better_actual'), range(10))]
        self.assertTrue(len(result) > 0, "Expected the output not to be empty")

    def test_running_start_heuristic_third_image(self):
        print("--- test_running_start_heuristic_third_image ---")
        start_interval = [271, 89]
        file = settings.files[0]
        edge_length = 6
        bounce_variance = 0
        path_algorithm = 'better_actual'
        check_length = 0
        result = np.array([i for i in rebound.run(
            file["img_file"], file["origin"], 0, start_interval, file["velocity"], file["target"], search_method="astar", max_iter=50000, log_level=logging.INFO, edge_length=edge_length, bounce_variance=bounce_variance, path_length=path_algorithm, consistency_check_length=check_length)])
        if result.size > 0:
            print(result)
            print(rebound.merge_intervals(list(result[:,0])))
        self.assertTrue(len(result) > 0, "Expected the output not to be empty")


if __name__ == '__main__':
    unittest.main()
