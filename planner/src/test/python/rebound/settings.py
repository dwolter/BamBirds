import os
from planner.src.main.python.rebound.settings import ABType

__dir_path__ = os.path.dirname(os.path.abspath(__file__))
__resources_dir__ = os.path.join(__dir_path__, "../../resources")
__scenes_dir__ = os.path.join(__resources_dir__, "scenes")

generator_default = "stop"

scene_files = [
    os.path.join(__scenes_dir__, "scene_1.png"),
    os.path.join(__scenes_dir__, "scene_2.png"),
    os.path.join(__scenes_dir__, "scene_3.png"),
    os.path.join(__scenes_dir__, "scene_4.png"),
    os.path.join(__scenes_dir__, "scene_5.png"),
    os.path.join(__scenes_dir__, "scene_6.png"),
    os.path.join(__scenes_dir__, "scene_7.png"),
    os.path.join(__scenes_dir__, "scene_8.png"),
    os.path.join(__scenes_dir__, "scene_9.png"),
    os.path.join(__scenes_dir__, "scene_10.png"),
    os.path.join(__scenes_dir__, "scene_11.png"),
    os.path.join(__scenes_dir__, "scene_12.png"),
]

files = [
    {"img_file": scene_files[0], "origin": (297, 331), "target": (
        522, 376), "velocity": 75, "target_type": ABType.Pig,
        "possible_intervals":[[41.25,43.25],[63.5,73.5]]},
    {"img_file": scene_files[1], "origin": (247, 332), "target": (
        571, 331), "velocity": 75, "target_type": ABType.Pig,
        "possible_intervals":[[20.75,27]]},
    {"img_file": scene_files[2], "origin": (320, 328), "target": (
        544, 320), "velocity": 75, "target_type": ABType.Pig,
        "possible_intervals":[[39.4,39.5],[39.88, 41.4],[47.7, 47.95]]},
    {"img_file": scene_files[3], "origin": (131, 336), "target": (
        238, 352), "velocity": 70, "target_type": ABType.Pig,
        "possible_intervals":[[-43,-38]]},
    {"img_file": scene_files[4], "origin": (166, 338), "target": (
        607, 378), "velocity": 72, "target_type": ABType.Pig,
        "possible_intervals":[[40.25,56.75]]},
    {"img_file": scene_files[5], "origin": (163, 340), "target": (
        695, 374), "velocity": 73, "target_type": ABType.Pig,
        "possible_intervals":[[33,40.75],[56.5,60.5]]},
    {"img_file": scene_files[6], "origin": (207, 220), "target": (
        725, 366), "velocity": 74, "target_type": ABType.Pig,
        "possible_intervals":[[37,45], [56.5,58.5], [59.5,60]]},
    {"img_file": scene_files[7], "origin": (131, 336), "target": (
        660, 366), "velocity": 70, "target_type": ABType.Pig,
        "possible_intervals":[[34,54.5]]},
    {"img_file": scene_files[8], "origin": (131, 336), "target": (
        358, 370), "velocity": 75, "target_type": ABType.Pig,
        "possible_intervals":[[38.5,42]]},
    {"img_file": scene_files[9], "origin": (131, 336), "target": (
        458, 331), "velocity": 75, "target_type": ABType.Pig,
        "possible_intervals":[[72.75,74]]},
    # # {"img_file": scene_files[4], "origin": [90, 0], "target": (358,370), "velocity": 50,"target_type": ABType.Pig},
    # # {"img_file": scene_files[1], "origin": [0, 5], "values": [[0, 90], [185, 365]]},
]