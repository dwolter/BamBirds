from typing import Final
from enum import Enum
import math

FLY: Final[int] = 0
BOUNCE: Final[int] = 1
SLIDE: Final[int] = 2
GOAL: Final[int] = 3

class ABType(Enum):
    """Enum for the object types in Angry Birds
    """
    Background = 0
    Ground = 1
    Hill = 2
    Sling = 3
    RedBird = 4 
    YellowBird = 5 
    BlueBird = 6
    BlackBird = 7
    WhiteBird = 8
    Pig = 9
    Ice = 10
    Wood = 11
    Stone = 12
    Duck = 13
    Edge = 14
    Watermelon = 15
    Trajectory = 16
    TNT = 18
    Unknown = -1

    def __eq__(self, value):
        if isinstance(value, ABType):
            return super().__eq__(value)
        return self.value == value

img_width = 840
img_height = 480

gravity = 9.81
restitution = 0.4
friction = 0.4

edge_length = 10

distance_to_target = 15

consistency_check_length = 5

bounce_variance = 0.0

bounce_variance_relative = False

number_of_results = 10

target_type = ABType.Pig

scene_name = "scene"

screen_scale = 1

image_debug=False
save_images=True

target_point = None

diagonal_edge_to_edge = math.sqrt(2*(edge_length/2)**2)
diagonal_zone_to_zone = math.sqrt(2*edge_length**2)

def init_variables():
    global diagonal_edge_to_edge, diagonal_zone_to_zone, distance_to_target
    
    diagonal_edge_to_edge = math.sqrt(2*(edge_length/2)**2)
    diagonal_zone_to_zone = math.sqrt(2*edge_length**2)
    distance_to_target = 10 + edge_length

