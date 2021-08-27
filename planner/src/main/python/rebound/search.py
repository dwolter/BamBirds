import logging
from math import dist
from queue import Queue, PriorityQueue
from . import settings, utils
from .state import State
from dataclasses import dataclass, field
from typing import Iterator
from PIL import Image
import numpy as np
from operator import itemgetter

log = logging.getLogger("rebound.search")

def bf_search(img: Image, px, state: State, *args, max_iter=100000, **kwargs) -> tuple:
    """Breadth-First search

    Arguments:
        img {Image} -- image for debugging
        px {numpy.ndarray} -- access to pixel values
        state {State} -- initial state

    Keyword Arguments:
        max_iter {int} -- maximum number of state expansions (default: {100000})

    Yields:
        tuple -- (goal state, iteration count)
    """
    qu = Queue()
    i = 0

    qu.put(state)
    while not qu.empty() and i < max_iter:
        v = qu.get()
        img.putpixel(v.coords,(0,0,255))
        for s in v.next_states(px):
            if s.mt == settings.GOAL:
                s.show_on_image(img,(0,0,255),(0,0,100))
                yield s, i
            qu.put(s)
        i += 1
    return


def df_search(img: Image, px, state: State, *args, max_iter=100000, **kwargs) -> tuple:
    """Depth-First search

    Arguments:
        img {Image} -- image for debugging
        px {numpy.ndarray} -- access to pixel values
        state {State} -- initial state

    Keyword Arguments:
        max_iter {int} -- maximum number of state expansions (default: {100000})

    Yields:
        tuple -- (goal state, iteration count)
    """
    
    img.putpixel(state.coords,(0,0,255))
    i = 0
    stack = []
    stack.append(state)
    while len(stack) > 0 and i < max_iter:
        v = stack.pop()
        img.putpixel(v.coords,(0,0,255))
        next_states = v.next_states(px)
        for s in next_states:
            if s.mt == settings.GOAL:
                s.show_on_image(img,(0,0,255),(0,0,100))
                yield s, i
            stack.append(s)
        i += 1
    log.debug(i)
    return

def manual_search(img: Image, px, state: State, *args, max_iter=100000, **kwargs) -> tuple:
    """Manual search

    Arguments:
        img {Image} -- image for debugging
        px {numpy.ndarray} -- access to pixel values
        state {State} -- initial state

    Keyword Arguments:
        max_iter {int} -- maximum number of state expansions (default: {100000})

    Yields:
        tuple -- (goal state, iteration count)
    """
    img.putpixel(state.coords,(0,0,255))
    i = 0
    tree = Node(state, None)
    current_node = tree
    try:
        while i < max_iter:
            if not current_node.children:
                for s in current_node.state.next_states(px, shuffle=False):
                    current_node.children.append(Node(s,current_node))
            while current_node.all_children_visited():
                current_node = current_node.parent
                if current_node == None:
                    log.info("All nodes visited and no solution found")
                    return

            while True:
                current_node.print_non_visited_children()
                user_input = input("> ")
                if user_input == "show":
                    current_node.state.show()
                elif user_input == "exit":
                    raise EOFError
                elif user_input == "help":
                    print("""Commands available: 
                    help  - display this message
                    show  - display current and all previous states
                    exit  - exit the manual search
                    [0-2] - select one of the possible next states
                    """)
                else:
                    try:
                        n = int(user_input)
                        if n in range(len(current_node.children)):
                            current_node = current_node.children[n]
                            break
                        else:
                            print("Sorry this number is not possible, please try again")
                    except:
                        print("Sorry this number is not possible, please try again")

            current_node.visited = True
            
            img.putpixel(current_node.state.coords,(0,0,255))
            if px[current_node.state.coords][0] == settings.ABType.Sling :
                yield current_node.state, i
            i += 1
    except (KeyboardInterrupt, EOFError) as e:
        print('exiting...')
    log.debug(i)
    return

def heuristic_astar(img: Image, px, state: State, heuristic, *heuristic_args, max_iter=100000, path_length_method=State.path_length) -> tuple:
    """A* Search. Uses a configurable heuristic. Additional arguments are passed to the heuristic

    Arguments:
        img {Image} -- image for debugging
        px {numpy.ndarray} -- access to pixel values
        state {State} -- initial state
        heuristic {function} -- function to calculate the heuristic of a state. The first argument passed are the states coordinates. additional arguments are also passed

    Keyword Arguments:
        max_iter {int} -- maximum number of state expansions (default: {100000})
        path_length_method {[type]} -- method for calculating the path length (default: {State.path_length}). The State is passed as first and only argument

    Yields:
        tuple -- (goal state, iteration count)
    """
    
    utils.put_pixel_save(img, state.coords, (None,None,255))
    i = 0

    queue = PriorityQueue()
    removed_states = []
    min_states_dict = {}

    queue.put(PrioritizedState(priority=heuristic(state.coords, *heuristic_args), g=0, item=state))

    while not queue.empty() and i < max_iter:
        i += 1
        ps = queue.get()
        # skip states with the same turn point
        if not state.no_gravity:
            item = id(ps.item.first_turn())
            while item in removed_states and not queue.empty():
                queue.task_done()
                ps = queue.get()
                item = id(ps.item.first_turn())
        v = ps.item
        if not utils.out_of_image(v.coords, True):
            utils.put_pixel_save(img, v.coords, (255,255,255))
        new_states = v.next_states(px, shuffle=False)
        for s in new_states:
            if s.mt == settings.GOAL:
                s.show_on_image(img,(255,255,255),(100,100,100), image_name="debug/{}-astar-{}-el{}-bv{}-cl{}-{}-i{}.png".format(settings.scene_name, heuristic.__name__, settings.edge_length, settings.bounce_variance, settings.consistency_check_length, path_length_method.__name__, i))
                yield s, i
                # Add the first turn point to removed states. Will prevent multiple numbers of very similar states
                removed_states.append(id(s.first_turn()))
                continue
            add = True
            h = heuristic(s.coords, *heuristic_args)
            # FIXME: hack to enable values very close to the target to be prioritized, regardless of their path length. 
            # Probably a better heuristic for the path length would be more useful, which uses some estimate for the overall path?
            if not state.no_gravity and h < settings.distance_to_target:
                g = 0
            else:
                g = path_length_method(s) 
            f = g + h
            prio_state = PrioritizedState(priority=f, g=g, item=s)
            if s.no_gravity:
                if s.coords[0] < state.coords[0]:
                    add = False
                else:
                    min_state = min_states_dict.get(s.coords,None)
                    if min_state is not None:
                        if g < min_state.g:
                            min_states_dict[s.coords] = prio_state
                            removed_states.append(id(min_state))
                        else:
                            add = False
                    else:
                        min_states_dict[s.coords] = prio_state
            if add:
                queue.put(prio_state)
        queue.task_done()
    if settings.image_debug:
        if settings.save_images:
            img.save("debug/{}-astar-{}-el{}-bv{}-cl{}-{}-i{}.png".format(settings.scene_name, heuristic.__name__, settings.edge_length, settings.bounce_variance, settings.consistency_check_length, path_length_method.__name__, i))
        else:
            img.show()
    return

def rrt(img: Image, px, state: State, heuristic, *heuristic_args, max_iter=100000, path_length_method=State.path_length) -> tuple:
    pass

def heuristic_best_first(img, px, state: State, heuristic, *heuristic_args, max_iter=100000, path_length_method=None) -> tuple:
    """Best-First Search. Uses a configurable heuristic. Additional arguments are passed to the heuristic

    Arguments:
        img {Image} -- image for debugging
        px {numpy.ndarray} -- access to pixel values
        state {State} -- initial state
        heuristic {function} -- function to calculate the heuristic of a state. The first argument passed are the states coordinates. additional arguments are also passed

    Keyword Arguments:
        max_iter {int} -- maximum number of state expansions (default: {100000})
        path_length_method {[type]} -- method for calculating the path length (default: {State.path_length}). The State is passed as first and only argument

    Yields:
        tuple -- (goal state, iteration count)
    """

    utils.put_pixel_save(img, state.coords, (None,None,255))
    i = 0

    queue = PriorityQueue()
    removed_states = []

    queue.put(PrioritizedState(priority=heuristic(state.coords, *heuristic_args), g=0, item=state))

    while not queue.empty() and i < max_iter:
        i += 1
        ps = queue.get()
        # skip states with the same turn point
        if not state.no_gravity:
            item = id(ps.item.first_turn())
            while item in removed_states and not queue.empty():
                queue.task_done()
                ps = queue.get()
                item = id(ps.item.first_turn())
        v = ps.item
        if not utils.out_of_image(v.coords, True):
            utils.put_pixel_save(img, v.coords, (255,255,255))
        new_states = v.next_states(px, shuffle=False)
        for s in new_states:
            if s.mt == settings.GOAL:
                s.show_on_image(img,(255,255,255),(100,100,100), image_name="debug/{}-best_first-{}-el{}-bv{}-cl{}-{}-i{}.png".format(settings.scene_name, heuristic.__name__, settings.edge_length, settings.bounce_variance, settings.consistency_check_length, path_length_method.__name__, i))

                yield s, i
                # Add the first turn point to removed states. Will prevent multiple numbers of very similar states
                removed_states.append(id(s.first_turn()))
                continue
            add = True

            h = heuristic(s.coords, *heuristic_args)
            if s.no_gravity:
                for state in queue.queue:
                    if state.item == s:
                        add = False
                        break
            prio_state = PrioritizedState(priority=h, g=0, item=s)
            if add:
                queue.put(prio_state)
    
    if settings.image_debug:
        if settings.save_images:
            img.save("debug/{}-best_first-{}-el{}-bv{}-cl{}-{}-i{}.png".format(settings.scene_name, heuristic.__name__, settings.edge_length, settings.bounce_variance, settings.consistency_check_length, path_length_method.__name__, i))
        else:
            img.show()
    return

def dijkstra_distance_matrix(pixels, img_width=settings.img_width, img_height=settings.img_height, edge_length=settings.edge_length) -> np.ndarray:
    dist = np.full((img_width, img_height), np.inf, dtype=np.float32)
    queue = PriorityQueue()
    target = (utils.floor_to_base(settings.target_point[0], edge_length), utils.floor_to_base(settings.target_point[1], edge_length))
    dist[target[0]:min(target[0]+edge_length, img_width),target[1]:min(target[1]+edge_length, img_height)] = 0
    queue.put(PrioritizedItem(priority=0, item=target))

    while not queue.empty():
        cur = queue.get().item

        for v, distance in utils.neighbours(cur):
            alt = dist[cur] + distance
            pixel_values = utils.get_pixel_save(pixels, v)
            if pixel_values is not None and (pixel_values[0] == settings.ABType.Background or pixel_values[0] == settings.target_type) and alt < dist[v]:
                dist[v[0]:min(v[0]+edge_length, img_width),v[1]:min(v[1]+edge_length, img_height)] = alt
                queue.put(PrioritizedItem(priority=alt, item=v))
    dist[dist == np.inf] = 1000
    return dist


@dataclass(order=True)
class PrioritizedState:
    """Wrapper for a comparable state
    """
    priority: float
    g: float=field(compare=False)
    item: State=field(compare=False)

@dataclass(order=True)
class PrioritizedItem:
    """Wrapper for a comparable item
    """
    priority: float
    item: field(compare=False)

class Node(object):
    """Wrapper node for a state tree

    Arguments:
        state {State} -- State of the node
        parent {Node} -- parent node
    """
    def __init__(self, state: State, parent):
        self.children = []
        self.state = state
        self.parent = parent
        self.visited = False

    def all_children_visited(self):
        """Returns if all children have been visited

        Returns:
            bool -- if all children have been visited
        """
        for c in self.children:
            if not c.visited:
                return False
        return True
    
    def print_non_visited_children(self):
        """Print not visited children
        """
        for i, c in enumerate(self.children):
            if not c.visited:
                print("{}: {}".format(i, c.state))

def matrix_heuristic(coords:tuple, matrix, *args) -> float:
    """Heuristic that uses the value in the matrix for the heuristic

    Arguments:
        coords {tuple} -- coordinates of the state
        matrix {np.ndarray} -- matrix with all heuristic values saved

    Returns:
        float -- value of the heuristic
    """
    try:
        return matrix[coords]
    except IndexError:
        # If coords is out of bounds return the nearest index in bounds + the distance to this index
        nearest_index = np.min([np.array(matrix.shape) - 1,coords],0)
        heuristic = matrix[nearest_index[0],nearest_index[1]]
        distance = np.max(np.array(coords) - nearest_index)
        return heuristic + distance
    

def distance_heuristic(coords: tuple, target: tuple, *args) -> float:
    """Wrapper for the euclidean distance of two arguments

    Arguments:
        coords {tuple} -- coordinates of the current state
        target {tuple} -- coordinates of the target

    Returns:
        float -- value of the heuristic
    """
    return dist(coords,target)
