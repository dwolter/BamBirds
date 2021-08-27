import unittest
import os
import planner.src.main.python.rebound as rebound
import numpy as np
import math

class TestUtils(unittest.TestCase):

    def test_bounce_equal_for_equal_wall_angles(self):
        print("--- bounce_equal_for_equal_wall_angles ---")
        P = [5,10]
        rg_start = [40,50]
        v_start = 10
        angles_cur = [30, 40]
        actual = [[320,330], 5]

        for wall_angle in range(0,541,180):
            res = rebound.calc_bounce_angles(P, rg_start, v_start, angles_cur, wall_angle)
            for actual_values, given_values in zip(actual, res):
                self.assertEqual(given_values, actual_values)


    def test_bounce_correct_for_arbitrary_walls(self):
        print("--- test_bounce_correct_for_arbitrary_walls ---")
        rg_in = [[100, 10],[170,10]]
        angles_in = [120, 175]

        res = rebound.calc_bounce_angles(rg_in, angles_in, 50)
        self.assertEqual(res, [[285,10], [340,10]])

        res = rebound.calc_bounce_angles(rg_in, angles_in, 80)
        self.assertEqual(res, [[345,10], [40,10]])

        # Because here the angle of the Object is between the possible angles it may come to problems. Do we care?
        res = rebound.calc_bounce_angles(rg_in, angles_in, 150)
        self.assertEqual(res, [[125,10], [180,10]])

        res = rebound.calc_bounce_angles(rg_in, angles_in, 193)
        self.assertEqual(res, [[211,10], [266,10]])

        res = rebound.calc_bounce_angles(rg_in, angles_in, 237)
        self.assertEqual(res, [[299,10], [354,10]])

    def test_interval_intersection(self):
        print("--- test_interval_intersection ---")
        tests = [
            {"expected": [0, 5], "values": [[185, 365], [0, 90]]},
            {"expected": [0, 5], "values": [[0, 90], [185, 365]]},
            {"expected": [0, 350], "values": [[0, 360], [0, 350]]},
            {"expected": [0, 90], "values": [[0, 360], [0, 90]]},
            {"expected": [90, 0], "values": [[0, 360], [90, 0]]},
            {"expected": [315, 5], "values": [[185, 5], [315, 45]]},
            {"expected": [75.93548644066307, 135], "values": [[45, 135], [75.93548644066307, 270.02827009141026]]}
        ]
        for test in tests:
            self.assertEqual(test["expected"], rebound.interval_intersection(
                test["values"][0], test["values"][1]))

    def test_interval_union(self):
        print("--- test_interval_union ---")
        tests = [
            {"expected": [185, 90], "values": [[185, 365], [0, 90]]},
            {"expected": [185, 90], "values": [[0, 90], [185, 365]]},
            {"expected": [0, 0], "values": [[0, 360], [0, 350]]},
            {"expected": [0, 0], "values": [[0, 360], [0, 90]]},
            {"expected": [0, 0], "values": [[0, 360], [90, 0]]},
            {"expected": [185, 45], "values": [[185, 5], [315, 45]]},
            {"expected": [45, 270.02827009141026], "values": [[45, 135], [75.93548644066307, 270.02827009141026]]}
        ]
        for test in tests:
            self.assertEqual(test["expected"], rebound.interval_union(
                test["values"][0], test["values"][1]))

    def test_interval_merge(self):
        print("--- test_interval_merge ---")
        tests = [
            {"expected": [[350, 25],[35,100],[110,120]], "values": [[35, 80], [75, 100], [110,120], [15,25], [350,17]]},
            {"expected": [[320, 80]], "values": [[35, 80], [320, 35]]},
            {"expected": [[35, 80]], "values": [[35, 80]]},
        ]
        for test in tests:
            self.assertEqual(test["expected"], rebound.merge_intervals(
                test["values"]))

    def test_possible_start_angles(self):
        print("--- test_possible_start_angle ---")
        tests = [
            {"expected": math.degrees(1.3422890385696), "values": [[[20, 50]], [35]]},
            {"expected": math.degrees(-1.3326421209863+math.pi), "values": [[[-2, 6]], [118]]},
            {"expected": math.degrees(-0.8170447276539+math.pi), "values": [[[-6, -2]], [240]]},
            {"expected": rebound.bound_angle(math.degrees(-0.6661089048849)), "values": [[[6, -8]], [298]]},
            {"expected": 90, "values": [[[0, 50]], [90]]}
        ]
        for test in tests:
            self.assertAlmostEqual(test["expected"], rebound.start_angle(
                test["values"][0], test["values"][1])[0])

    def test_impossible_start_angles(self):
        print("--- test_impossible_start_angle ---")
        tests = [
            {"expected": ArithmeticError, "values": [[[20, 50]], [70]]},
            {"expected": ValueError, "values": [[[20, 50]], [90]]},
            {"expected": ValueError, "values": [[[0.5, -50]], [270]]},
        ]
        for test in tests:
            self.assertRaises(test["expected"], rebound.start_angle, test["values"][0], test["values"][1])

    def test_possible_start_velocities(self):
        print("--- test_possible_start_velocities ---")
        tests = [
            {"expected": 32.5918732704102, "values": [[[20, 50]], [math.degrees(1.3422890385696)]]},
            {"expected": 31.32091953, "values": [[[0, 50]], [90]]},
            {"expected": 0, "values": [[[0, -50]], [270]]}
        ]
        for test in tests:
            self.assertAlmostEqual(test["expected"], rebound.start_velocity(
                test["values"][0], test["values"][1])[0])

    def test_impossible_start_velocities(self):
        print("--- test_impossible_start_velocities ---")
        tests = [
            [[[20, 50]], [math.degrees(0.5892874345567)]],
            [[[20, 50]], [90]],
            [[[20, 50]], [270]]
        ]
        for test in tests:
            self.assertRaises(ValueError, rebound.start_velocity, test[0], test[1], 9.81)

    def test_possible_angles(self):
        print("--- test_possible_angles ---")
        tests = [
            {"expected": [270, 0], "values": [0, 1]},
            {"expected": [315, 45], "values": [0, 2]},
            {"expected": [0, 90], "values": [0, 3]},
            {"expected": [90, 270], "values": [0, 0]},
            {"expected": [90, 180], "values": [1, 0]},
            {"expected": [180, 0], "values": [1, 1]},
            {"expected": [0, 90], "values": [1, 2]},
            {"expected": [45, 135], "values": [1, 3]},
            {"expected": [135, 225], "values": [2, 0]},
            {"expected": [180, 270], "values": [2, 1]},
            {"expected": [270, 90], "values": [2, 2]},
            {"expected": [90, 180], "values": [2, 3]},
            {"expected": [180, 270], "values": [3, 0]},
            {"expected": [225, 315], "values": [3, 1]},
            {"expected": [270, 0], "values": [3, 2]},
            {"expected": [0, 180], "values": [3, 3]}
        ]
        for test in tests:
            self.assertEqual(test["expected"], rebound.possible_angles(
                test["values"][0], test["values"][1]))


    def test_possible_current_angles(self):
        print("--- test_possible_current_angles ---")
        tests = [
            {"expected": 300, "values": [[[math.degrees(1.3387602796903),15.7745021503418]], [[8,10]]]},
            {"expected": 42, "values": [[[math.degrees(1.126302420656),13.3030330084119]], [[4,6]]]},
            {"expected": 236, "values": [[[math.degrees(1.878264529),16.6577001875953]], [[-12,10]]]},
            {"expected": 90, "values": [[[90,15]], [[0,10]]]},
            {"expected": 270, "values": [[[90,13]], [[0,10]]]},
            {"expected": 270, "values": [[[270,13]], [[0,-10]]]}
        ]
        for test in tests:
            self.assertAlmostEqual(test["expected"], rebound.current_angle(
                test["values"][0], test["values"][1])[0])

    def test_impossible_current_angles(self):
        print("--- test_impossible_current_angles ---")
        tests = [
            {"expected": ArithmeticError, "values": [[[math.degrees(1.3387602796903),0]], [[8,10]]]},
            {"expected": ValueError, "values": [[[90,15]], [[10,10]]]},
            {"expected": ValueError, "values": [[[90,13]], [[-1,10]]]},
            {"expected": ValueError, "values": [[[270,13]], [[0.5,-10]]]}
        ]
        for test in tests:
            self.assertRaises(test["expected"], rebound.current_angle,
                test["values"][0], test["values"][1])


    def test_convert_coordinates(self):
        print("--- test_convert_coordinates ---")
        tests = [
            {"expected": [5,5], "values": [[5,-5]]},
            {"expected": [0,0], "values": [[0,0]]},
            {"expected": [10,0], "values": [[10,0]]},
            {"expected": [24,-10], "values": [[24,10]]}
        ]
        for test in tests:
            self.assertEqual(test["expected"], rebound.convert_coordinates(
                test["values"][0]))

    def test_edge_coordinates(self):
        print("--- test_edge_coordinates ---")
        tests = [
            {"expected": [9.5,10], "values": [[10,10],0]},
            {"expected": [10,10.5], "values": [[10,10],1]},
            {"expected": [10.5,10], "values": [[10,10],2]},
            {"expected": [10,9.5], "values": [[10,10],3]}
        ]
        for test in tests:
            self.assertEqual(test["expected"], rebound.edge_coordinates(
                test["values"][0], test["values"][1]))

    def test_minimum_velocity(self):
        print("--- test_minimum_velocity ---")
        tests = [
            {"expected": 11.3842398088148, "values": [[4,6]]},
            {"expected": 16.6154852613097, "values": [[2,14]]},
            {"expected": 13.7647186812895, "values": [[-8,8]]},
            {"expected": 5.6125676855254, "values": [[-6,-4]]},
            {"expected": 0, "values": [[0,0]]}
        ]
        for test in tests:
            self.assertAlmostEqual(test["expected"], rebound.minimum_velocity(
                test["values"][0]))

    def test_hits_edge(self):
        print("--- test_hits_edge ---")
        tests = [
            {"expected": True, "values": [[math.degrees(1.4937050181839),17],[4,6]]},
            {"expected": True, "values": [[math.degrees(1.059885031853),17],[4,6]]},
            {"expected": True, "values": [[math.degrees(-1.4925998887517),22.9],[-8,4]]},
            {"expected": True, "values": [[math.degrees(-0.2087418569086),5.1],[4,-4]]},
            {"expected": True, "values": [[math.degrees(0.9226227030549),8.9],[-4,-8]]},
            {"expected": False, "values": [[math.degrees(1.059885031853),17],[4,8]]},
            {"expected": False, "values": [[math.degrees(1.059885031853),17],[7,6]]},
            {"expected": False, "values": [[math.degrees(-1.4925998887517),22.9],[-8,-4]]},
            {"expected": False, "values": [[math.degrees(-0.2087418569086),5.1],[3,-4]]},
            {"expected": False, "values": [[math.degrees(0.9226227030549),8.9],[-4,-10]]},
            {"expected": True, "values": [[90,15],[0,10]]},
            {"expected": True, "values": [[270,0],[0,-10]]},
            {"expected": False, "values": [[90,13],[0,10]]},
            {"expected": False, "values": [[90,15],[1,10]]},
            {"expected": False, "values": [[90,15],[0,-10]]},
            {"expected": False, "values": [[270,15],[0,10]]},
            {"expected": False, "values": [[270,15],[1,-10]]}
        ]
        for test in tests:
            self.assertEqual(test["expected"], rebound.hits_edge(
                test["values"][0], test["values"][1]))

    def test_angle_to_hit(self):
        print("--- test_angle_to_hit ---")
        tests = [
            {"expected": math.degrees(1.4937050181839), "values": [17,[4,6],True]},
            {"expected": math.degrees(1.059885031853), "values": [17,[4,6],False]},
            {"expected": rebound.bound_angle(math.degrees(-1.4925998887517 + math.pi)), "values": [22.9,[-8,4],True]}
        ]
        for test in tests:
            self.assertAlmostEqual(test["expected"], rebound.angle_to_hit(
                test["values"][0], test["values"][1], test["values"][2]))

    def test_correct_angle_order(self):
        print("--- test_angle_to_hit ---")
        tests = [
            {"expected": True, "values": [45, 90]},
            {"expected": True, "values": [340, 25]},
            {"expected": True, "values": [181, 0]},
            {"expected": True, "values": [0, 180]},
            {"expected": True, "values": [180, 0]},
            {"expected": False, "values": [90, 45]},
            {"expected": False, "values": [25, 345]},
            {"expected": False, "values": [0, 181]},
        ]
        for test in tests:
            print(test)
            self.assertEqual(test["expected"], rebound.correct_angle_order(
                test["values"]))


    def test_edge_angles(self):
        print("--- test_edge_angles ---")
        tests = [
            {"expected": [37,171], "values": [105, 37, 122, 171]},
            {"expected": [67, 157], "values": [157, 67]},
            {"expected": [299, 72], "values": [318, 299, 72, 47]},
            {"expected": [334, 55], "values": [334, 47, 7, 55]},
        ]
        
        for test in tests:
            print(test)
            self.assertEqual(test["expected"], rebound.edge_angles(
                test["values"]))


if __name__ == '__main__':
    unittest.main()

