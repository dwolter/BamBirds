from .state import State
from .utils import *
from .main import run, main
from .search import *
import logging
import argparse
import ast
import os

logging.basicConfig(level=logging.DEBUG, filename="logs/rebound.log", filemode="a", format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

def create_argparser(parser: argparse.ArgumentParser):
    subparsers = parser.add_subparsers(title='commands', description='available commands for the rebound parser')
    run = subparsers.add_parser(
        'run', help='Run the rebound planner')
    run.add_argument('--search', type=str, default='astar', help='Search strategy for rebound planning. Options: bf (breadth-first, default), df (depth-first), manual')
    run.add_argument('--start', type=ast.literal_eval, help='Start point for the simulation. Tuple format and two elements')
    run.add_argument('--target', type=ast.literal_eval, help='Target point for the simulation. Tuple format and two elements')
    run.add_argument('--image', type=str, default='scene_11.png', help='Image for evaluation. Lossless format where R=ABType, G=EdgeAngle')
    run.add_argument('--angle_range', type=ast.literal_eval, default='[271,89]', help='Range of angles. list or tuple of length 2')
    run.add_argument('--velocity', type=float, default=75, help='Starting volocity in pixel/s')
    run.add_argument('--gravity', type=float, default=9.81, help='Gravity in pixel^2/s')
    run.add_argument('--edge_length', type=int, default=10, help='Edge length for the evaluation. Not all pixels are evaluated, but only in the distance of edge_length')
    run.add_argument('--restitution', type=float, default=0.4, help='Restition of the ball. 1 means no reduction in velocity on bounce')
    run.add_argument('--friction', type=float, default=0.4, help='The friction on impact with objects. 1 means no friction')
    run.add_argument('--maxiter', type=int, default=10000, help='Maximum iterations performed for each search')
    run.add_argument('--type', type=int, default=9, help='ABType of the target object')
    run.add_argument('--bounce_variance', type=float, default=0.0, help='Variance of angles after a rebound')
    run.add_argument('--consistencylength', type=int, default=5, help='Length of states checked for consistency with the current angle and velocity')
    run.add_argument('--path_length', type=str, default='default', help='Which path length to use. Options: default (distance between each state), better (fro no gravity default, otherwise distance between turn points), actual (same as default except for three states forming an edge. there dist), better_actual (actual plus improvement from better)')

    run.set_defaults(func=main)

    try:
        import planner.src.test.python.rebound.generate_data as generate_data
    
        generate = subparsers.add_parser(
            'generate', help='Generate evaluation data for the rebound planner')
            
        generate.add_argument('--files', choices=[str(x) for x in range(1,11,1)], default=[str(x) for x in range(1,11,1)], nargs='*', help='Level IDs to play')
        generate.add_argument('--iterations', type=int, default=50000, help='Number of iterations')
        generate.add_argument('--theta0_min', type=int, default=271, help='Minimal start angle')
        generate.add_argument('--theta0_max', type=int, default=89, help='Maximal start angle')
        generate.add_argument('--random', action='store_true', help='Select combinations randomly')
        generate.add_argument('--random_avg_per_file',type=int, default=50, help='Average number of randomly selected combinations per level')
        generate.add_argument('--el_min', type=int, default=6, help='Minimal edge length')
        generate.add_argument('--el_max', type=int, default=12, help='Maximal edge length')
        generate.add_argument('--cl_min', type=int, default=0, help='Minimal check length')
        generate.add_argument('--cl_max', type=int, default=0, help='Maximal check length')
        generate.add_argument('--bv_min', type=int, default=0, help='Minimal bounce variance')
        generate.add_argument('--bv_max', type=int, default=0, help='Maximal bounce variance')
        generate.add_argument('--bv_abs', action='store_true', help='Absolute bounce variance. Relative is default')
        generate.add_argument('--bv_num', type=int, default=1, help='Number of random creations of bounce variance values')
        generate.add_argument('--path_algorithms', choices=['actual', 'better_actual'], default=['actual', 'better_actual'], nargs='*', help='Which path length algorithms to use')
        generate.add_argument('--search_methods', choices=['astar', 'best_first'], default=['astar', 'best_first'], nargs='*', help='Which path length algorithms to use')
        generate.add_argument('csv_file', type=os.path.abspath, help='Which path length algorithms to use')

        generate.set_defaults(func=generate_data.main)
    except ImportError:
        pass
