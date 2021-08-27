import logging
from .state import State
from . import search, settings, utils
from PIL import Image
import os
import numpy as np

log = logging.getLogger("rebound")

generator_default = "stop"

def run(img_file: str, start_point: tuple, entry_edge: int, angle_interval: list, velocity: float, target: tuple, search_method="astar", max_iter=50000, log_level=logging.INFO, restitution=settings.restitution, friction=settings.friction, edge_length=settings.edge_length, gravity=settings.gravity, target_type=settings.target_type, consistency_check_length=0, bounce_variance=0, path_length="actual", bounce_variance_relative=True):
    """run the rebound planner

    Arguments:
        img_file {str} -- path to the representation image
        start_point {tuple} -- origin point (will normally be the pivot of the slignshot)
        entry_edge {int} -- Entry edge for of the origin point
        angle_interval {list} -- initial angle interval
        velocity {float} -- starting velocity
        target {tuple} -- coordinates of the target

    Keyword Arguments:
        search_method {str} -- algorithm for the search (default: {"astar"})
        max_iter {int} -- maximum number of iterations for the searches (default: {50000})
        log_level {int} -- level for logging (default: {logging.INFO})
        restitution {float} -- value for the restitution used in rebounds (default: {settings.restitution})
        friction {float} -- value for the friction used in rebounds and sliding (default: {settings.friction})
        edge_length {int} -- edge length of a zone (default: {settings.edge_length})
        gravity {float} -- value for the gravity used in flying and sliding (default: {settings.gravity})
        target_type {ABType} -- type of the target object (default: {settings.target_type})
        consistency_check_length {int} -- number of previous edges checked to be in a new interval (default: {0})
        bounce_variance {int} -- variance added after a bounce (default: {0})
        path_length {str} -- method used for the path length (default: {"default"})
        bounce_variance_relative {bool} -- is the bounce variance relative to the possible angles or in absolute degrees (default: {True})

    Raises:
        ValueError: if the search method is not supported

    Yields:
        tuple -- (angle_interval, first turn point, iterations needed)
    """
    # init logging
    log.setLevel(level=log_level)
    logging.getLogger().setLevel(level=log_level)
    
    # init parameters
    settings.restitution = restitution
    settings.friction = friction
    settings.edge_length = edge_length
    settings.gravity = gravity
    settings.target_type = settings.ABType(target_type)
    settings.target_point = target
    settings.scene_name = os.path.basename(img_file).replace(".png","")
    settings.consistency_check_length = consistency_check_length
    settings.bounce_variance = bounce_variance
    settings.bounce_variance_relative = bounce_variance_relative
    settings.init_variables()

    # load image
    img = Image.open(img_file)
    settings.img_width, settings.img_height = img.size

    start_point = (
        utils.floor_to_base(start_point[0], edge_length),
        utils.floor_to_base(start_point[1], edge_length)
    )

    # get numpy array rather than pixel access because of weird problems
    # transpose because x and y axis get switched
    px = np.asarray(img).transpose([1,0,2])
    px = utils.shrink_resolution(px, edge_length)
    img = Image.fromarray(px).transpose(Image.ROTATE_270).transpose(Image.FLIP_LEFT_RIGHT)


    # set starting point
    state = State(None,None,start_point,entry_edge,settings.FLY,angle_interval,velocity)

    if path_length == "better":
        path_length_method = State.better_path_length
    elif path_length == "actual":
        path_length_method = State.actual_path_length
    elif path_length == "better_actual":
        path_length_method = State.better_actual_path_length
    else:
        path_length_method = State.actual_path_length

    if search_method == "astar":
        search_function = search.heuristic_astar
    elif search_method == "best_first":
        search_function = search.heuristic_best_first
    elif search_method == "df":
        search_function = search.df_search
    elif search_method == "bf":
        search_function = search.bf_search
    elif search_method == "manual":
        search_function = search.manual_search
    # run search alg with simulation
    if search_method in ["df", "bf", "manual"]:
        result = search_function(img, px, state, max_iter=max_iter)
    elif search_method in ["astar", "best_first"]:
        ####
        # Coarse heuristic path search
        ####
        
        state_no_gravity = State(None,None,start_point,entry_edge,settings.FLY,None,None, no_gravity=True)

        ####
        # Matrix for distance to coarse path
        ####

        distance_heuristic_matrix = search.dijkstra_distance_matrix(px,settings.img_width, settings.img_height, settings.edge_length)

        if settings.image_debug:
            distance_image = Image.fromarray(((distance_heuristic_matrix/np.max(distance_heuristic_matrix))* 255).astype('uint8'),mode='L').transpose(Image.ROTATE_270).transpose(Image.FLIP_LEFT_RIGHT)
            if settings.save_images:
                distance_image.save("debug/{}-{}-distance.png".format(settings.scene_name, settings.edge_length))
            else:
                distance_image.show()


        ####
        # Search now with distance to coarse path as heuristic
        ####

        result_iterator = search_function(img.copy(), px, state, search.matrix_heuristic, distance_heuristic_matrix, max_iter=max_iter, path_length_method=path_length_method)
        
        result = []
        next_result = next(result_iterator, generator_default)
        while next_result != generator_default:
            result_state, iteration = next_result
            turn_point = result_state.first_turn()
            if turn_point == None:
                turn_point = state
            yield result_state.start_angle_range(), turn_point.coords, iteration
            next_result = next(result_iterator, generator_default)
        
        return None
        yield
    
    else:
        raise ValueError("Given search_method not supported")
 
    return result

def main(args):
    """Entrypoint to rebound planner with argparse arguments. Prints results once they occur to stdout.

    Arguments:
        args {argparse.Namespace} -- Arguments parsed by argparse

    Raises:
        ValueError: for invalid image, start/target point, angle range or edge length
        TypeError: if  start/target point or angle range ar not a list/tuple

    Returns:
        list -- a list of possible angle ranges
    """
    __dir_path__ = os.path.dirname(os.path.abspath(__file__))
    __resources_dir__ = os.path.join(__dir_path__, "../../../test/resources")
    __scenes_dir__ = os.path.join(__resources_dir__, "scenes")
    
    if not os.path.exists('logs'):
        os.mkdir('logs')
    if not os.path.exists('debug'):
        os.mkdir('debug')

    if os.path.isfile(args.image):
        image_file = args.image
    elif os.path.isfile(os.path.join(__scenes_dir__, args.image)):
        image_file = os.path.join(__scenes_dir__, args.image)
    else:
        raise ValueError('image file not found')

    if isinstance(args.start, tuple):
        if len(args.start) >= 2:
            if isinstance(args.start[0], int) and isinstance(args.start[1], int):
                start = args.start[:2]
            else:
                raise ValueError("Values in start need to be integers")
        else:
            raise ValueError("start should be at least 2 values long")
    else:
        raise TypeError("start should be a tuple")

    if isinstance(args.target, tuple):
        if len(args.target) >= 2:
            if isinstance(args.target[0], int) and isinstance(args.target[1], int):
                target = args.target[:2]
            else:
                raise ValueError("Values in target need to be integers")
        else:
            raise ValueError("target should be at least 2 values long")
    else:
        raise TypeError("target should be a tuple")

    if isinstance(args.angle_range, (tuple, list)):
        if len(args.angle_range) >= 2:
            if isinstance(args.angle_range[0], (int, float)) and isinstance(args.angle_range[1], (int, float)):
                angle_range = args.angle_range[:2]
            else:
                raise ValueError("Values in angle_range need to be integers or floats")
        else:
            raise ValueError("angle_range should be at least 2 values long")
    else:
        raise TypeError("angle_range should be a tuple or list")

    if args.edge_length <= 0:
        raise ValueError("edge_length should be at least 1")

    if args.verbose:
        log_level = logging.DEBUG
    else:
        log_level = logging.ERROR
    
    result_iterator = run(image_file, start, 1, angle_range, args.velocity, target, search_method=args.search, max_iter=args.maxiter, log_level=log_level, restitution=args.restitution, friction=args.friction, edge_length=args.edge_length, gravity=args.gravity, target_type=settings.ABType(args.type), consistency_check_length=args.consistencylength, bounce_variance=args.bounce_variance, path_length=args.path_length)
        
    result = []
    next_result = next(result_iterator, generator_default)
    while next_result != generator_default:
        print(next_result)
        result.append(next_result)
        next_result = next(result_iterator, generator_default)
    return result
