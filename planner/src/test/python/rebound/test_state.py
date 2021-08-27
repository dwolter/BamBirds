import unittest
import os

__dir_path__ = os.path.dirname(os.path.abspath(__file__))
__resources_dir__ = os.path.join(__dir_path__, "../../resources")
__scenes_dir__ = os.path.join(__resources_dir__, "scenes")

scene_files = [
    os.path.join(__scenes_dir__, "scene_15.png"),
    os.path.join(__scenes_dir__, "scene_16.png")
]

class TestState(unittest.TestCase):

    def test_apply_gravity(self):
        pass


if __name__ == '__main__':
    unittest.main()
