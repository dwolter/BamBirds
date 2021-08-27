import logging
import math
import random
import copy
from matplotlib import pyplot as plt
import numpy as np
from . import settings, utils

log = logging.getLogger("rebound.State")

class State(object):
    """Defines a state in the simulation

    Arguments:
        turn {State} -- Last State where the direction changed because of a bounce.
        prev {State} -- Previous State
        coords {tuple} -- coordinates of the state
        entry_edge {int} -- edge on which the current zone was entered
        mt {int} -- Movement Type. One of settings.[FLY|BOUNCE|SLIDE]
        theta_range {list} -- Angle interval at turn to reach this state
        velocity {float} -- velocity at turn 

    Keyword Arguments:
        no_gravity {bool} -- if no gravity, angle, rebound calculations should be executed (default: {False})

    Raises:
        TypeError: invalid type for coords, entry_edge or mt
    """
    
    def __init__(self, turn, prev, coords: tuple, entry_edge: int, mt: int, theta_range: list, velocity: float, no_gravity=False):
        if coords == None or not isinstance(coords, tuple):
            raise TypeError("coords should be a tuple and not {}".format(coords))
        if entry_edge == None or not isinstance(entry_edge, int):
            raise TypeError("entry edge should be an int and not {}".format(entry_edge))
        if mt == None or not isinstance(mt, int):
            raise TypeError("movement type should be an int and not {}".format(mt))
        self.turn = turn
        self.prev = prev
        self.coords = coords
        self.entry_edge = entry_edge
        self.mt = mt
        self.theta_range = theta_range
        self.velocity = velocity
        self.no_gravity = no_gravity

    def next_states(self, pixels, shuffle=True):
        log.debug("")
        log.debug("")
        log.debug("### next states of {}".format(str(self)))

        result = []

        ### next edge clockwise
        result.append(self.next_state(utils.cw(self.entry_edge), pixels))

        #### edge parallel
        result.append(self.next_state(utils.par(self.entry_edge), pixels))

        #### next edge counterclockwise
        result.append(self.next_state(utils.ccw(self.entry_edge), pixels))

        #### current edge?
        # result.append(self.next_state(self.entry_edge, pixels))

        # Remove all None values from list
        # They get returned if the edge is not reachable
        while None in result:
            result.remove(None)
        for state in result:
            s = state
            j = 0
            while s is not None and j < 2:
                s = s.prev
                if s == state:
                    result.remove(state)
                    break
                j += 1
        if not result:
            log.info('No possible following states for %s',self)
            # Uncomment for debugging
            # self.next_state(utils.cw(self.entry_edge), pixels)
            # self.next_state(utils.par(self.entry_edge), pixels)
            # self.next_state(utils.ccw(self.entry_edge), pixels)
        if shuffle:
            random.shuffle(result)
        return result

    def next_state(self, exit_edge: int, pixels):
        """Calculate the a next state

        Arguments:
            exit_edge {int} -- edge to exit the current zone
            pixels {numpy.ndarray} -- matrix for representation

        Returns:
            State -- the new state in the zon of exit edge
        """
        
        new_coords = self.next_pixel(exit_edge)
        if utils.out_of_image(new_coords):
            return None

        try:
            if self.no_gravity:
                new_r, current_angles = None, None
            elif self.mt == settings.SLIDE:
                pass
            else:
                possible_angle_range = utils.possible_angles(self.entry_edge, exit_edge)
                new_r, current_angles = self.apply_gravity(new_coords, utils.par(exit_edge), possible_angle_range)
        except (ValueError, ArithmeticError, ZeroDivisionError) as e:
            # log.exception(e)
            return None
        
        # radians = math.atan2(self.coords[1]-self.turn[1],self.coords[0]-self.turn[0])
        # actual_angle = bound_angle((180 / math.pi) * radians)
        
        # now calculate the acutally possible range
        # new_angle_range = ranges_overlap(self.angle_range, posible_angle_range, actual_angle=actual_angle)
        # if not new_angle_range:
        #     return None

        # log.debug((Θ_1, new_angle_range[1]))

        # Now we need to calculate the next state 
        turn = self if self.turn is None else self.turn
        try:
            if self.hits_target(pixels, exit_edge):
                # If we hit the target we can return
                return State(turn,self.prev,self.coords,self.entry_edge, settings.GOAL, self.theta_range, self.velocity, self.no_gravity)
            elif self.mt == settings.FLY or self.no_gravity:
                # If the next edge is free we can continue flying
                if self.free(pixels, exit_edge):
                    # return new state with new_angle_range
                    return State(turn, self, new_coords, utils.par(exit_edge), settings.FLY, new_r, self.velocity, self.no_gravity)
                elif self.no_gravity:
                    return None
                else: # Bounce

                    new_r, velocity, bounce_edge = self.bounce(new_r, exit_edge, self.point_angle(pixels, exit_edge))
                    # TODO: should we set state here to bounce or fly?
                    #
                    # bounce would guarantee us that we know that we came from bouncing and if 
                    # we can continue on the current edge, we can start sliding
                    # 
                    # but what are other implications?
                    #
                    return State(self, self, self.coords, bounce_edge, settings.BOUNCE, new_r, velocity, self.no_gravity)
                    # return State(self.coords, self, self.coords,exit_edge,BOUNCE,(Θ_1, Θ_2), new_velocity_range)
                
            elif self.mt == settings.BOUNCE:
                # If the next edge is free we can start flying
                if self.free(pixels, exit_edge):
                    return State(self, self, new_coords,utils.par(exit_edge),settings.FLY, new_r, self.velocity, self.no_gravity)
                else:
                    # FIXME: Sliding does not work yet
                    # try:
                    #     bounced_point_angle = self.point_angle(pixels,self.entry_edge)
                    #     if bounced_point_angle[1] < 0:
                    #         raise ValueError
                    #     point_angle_entry = utils.vector_angle_between((0,1), bounced_point_angle)
                    #     point_angle_entry = [point_angle_entry, utils.bound_angle(point_angle_entry+180)]
                    # except ValueError:
                    #     point_angle_entry = [1000,1000]
                    # isclose = tuple(np.isclose(current_angles,point_angle_entry,atol=10))
                    isclose = [False]
                    # If the new angle is close to the angle of the wall (or the velocity is low?) and the wall does not hang over start sliding
                    if any(isclose):
                        # TODO: maybe recalculate new_v 

                        # TODO: what happens if new_theta goes in both ways of the edge? 

                        # TODO: what is the next pixel anyway?

                        return State(turn, self, new_coords, utils.par(exit_edge), settings.SLIDE, [point_angle_entry[isclose.index(True)] for i in range(2)], self.velocity, self.no_gravity,)
                    else: # Bounce again
                        new_r, velocity, bounce_edge = self.bounce(new_r, exit_edge, self.point_angle(pixels, exit_edge))
                        
                        # TODO: should we set state here to bounce or fly?
                        #
                        # bounce would guarantee us that we know that we came from bouncing and if 
                        # we can continue on the current edge, we can start sliding
                        # 
                        # but what are other implications?
                        #
                        return State(self, self, self.coords, bounce_edge, settings.BOUNCE, new_r, velocity, self.no_gravity)
                        # return State(self.coords, self, self.coords,exit_edge,BOUNCE,(Θ_1, Θ_2), new_velocity_range)

                pass
            elif self.mt == settings.SLIDE:
                try:
                    bounced_point_angle = self.point_angle(pixels,self.entry_edge)
                    if bounced_point_angle[1] < 0:
                        raise ValueError
                    point_angle_entry = utils.vector_angle_between((0,1), bounced_point_angle)
                    point_angle_entry = [point_angle_entry, utils.bound_angle(point_angle_entry+180)]
                except ValueError:
                    point_angle_entry = [1000,1000]

                # TODO: Problem here because the next edge calculated is not one we can slide on? 
                # We need to check for the next other edge that is connected to this edge over the 
                # common vertex at the correct side and is solid

                # calculate next solid edge on the vertex in direction of angle_range 
                # Here we probably need the https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
                log.warning("-- not implemented")
                return None
        except (IndexError, ValueError, ArithmeticError) as e:
            # log.exception(e)
            pass
        return None

    def bounce(self, start_angles, bounce_edge: int, wall_angle):
        """bounce and calculate the angles after a bounce

        Arguments:
            start_angles {sequence} -- starting angles at turn
            bounce_edge {int} -- edge of the bounce
            wall_angle {np.ndarray} -- normal for the wall

        Returns:
            list -- new angle interval after the bounce
        """
        new_r, new_v = utils.calc_bounce_angles(
            utils.relative_point(
                utils.edge_coordinates(
                    utils.convert_coordinates(self.turn.coords),
                    self.turn.entry_edge
                ), 
                utils.edge_coordinates(
                    utils.convert_coordinates(self.coords), 
                    bounce_edge
                )
            ), 
            start_angles, 
            self.velocity,
            wall_angle
        )
        if utils.intervals_intersect(new_r,utils.possible_angles(bounce_edge, bounce_edge), adapt_angles=True):
            # Check if the bounce edge should be a different one
            
            i = utils.cw(bounce_edge)
            candidates = []
            while i != bounce_edge:
                if utils.intervals_intersect(new_r,utils.possible_angles(i, utils.par(i)), adapt_angles=True):
                    candidates.append(i)
                i = utils.cw(i)
            if not candidates:
                # List should acutally never be empty
                pass
            elif len(candidates) == 1:
                bounce_edge = candidates[0]
            else:
                #TODO: not sure what to do if multiple edges are possible?
                bounce_edge = candidates[0]

        return new_r, new_v, bounce_edge

    def on_line(self):
        #TODO is this still needed?
        if self.coords[0] == self.turn[0]:
            return True
        
        m = (self.coords[1] - self.turn[1]) / (self.coords[0] - self.turn[0])

        t = -m * self.coords[0] + self.coords[1]

        prev = self.prev
        while prev != None and prev.coords != self.turn:
            y = m * prev.coords[0] + t
            if abs(y-prev.coords[1]) > 1:
                return False
            prev = prev.prev
        return True

    def next_pixel(self, i: int):
        """Calculate the coordinates of the next pixel at exit edge i

        Arguments:
            i {int} -- exit edge

        Returns:
            tuple -- coordinates of the next pixel
        """
        i = i % 4
        if i == 0:
            return (self.coords[0] - settings.edge_length, self.coords[1])
        elif i == 1:
            return (self.coords[0], self.coords[1] + settings.edge_length)
        elif i == 2:
            return (self.coords[0] + settings.edge_length, self.coords[1])
        elif i == 3:
            return (self.coords[0], self.coords[1] - settings.edge_length)

    def free(self, pixels, i=None):
        """Calculate if the current or next zone is only filled with ABType.Background

        Arguments:
            pixels {numpy.ndarray} -- level representation

        Keyword Arguments:
            i {int} -- if set the zone at the exit edge i is checked (default: {None})

        Returns:
            bool -- if the zone is free
        """
        if not i is None:
            next_pixel = self.next_pixel(i)
        else:
            next_pixel = self.coords
        return not utils.check_zone(pixels, next_pixel, 0, lambda x: x != settings.ABType.Background)

    def hits_target(self, pixels, i=None):
        """Calculate if the corrent or next zone contains hits the target

        Arguments:
            pixels {numpy.ndarray} -- level representation

        Keyword Arguments:
            i {int} -- if set the zone at the exit edge i is checked (default: {None})

        Returns:
            bool -- if the zone hits the target
        """
        if not i is None:
            next_pixel = self.next_pixel(i)
        else:
            next_pixel = self.coords
        if math.dist(next_pixel, settings.target_point) < settings.distance_to_target:
            if utils.check_zone(pixels, next_pixel, 0, lambda x: x == settings.target_type):
                return not utils.hill_between(pixels, next_pixel, settings.target_point)
        return False

    def point_angle(self, pixels, i=None) -> np.ndarray:
        """Calculate the angle of the wall in 

        Arguments:
            pixels {numpy.ndarray} -- level representation

        Keyword Arguments:
            i {int} -- if set the zone at the exit edge i is checked (default: {None})

        Raises:
            ValueError: if there is no object edge in the zone

        Returns:
            np.ndarray -- the normal of the edge at the specified zone
        """
        if not i is None:
            next_pixel = self.next_pixel(i)
        else:
            next_pixel = self.coords
        result = utils.check_zone(pixels, next_pixel, [1,2], lambda x: 0 if list(x) == [0,0] else list(x))
        if not result:
            raise ValueError('There is no edge at the next pixel in direction '+str(i))
        result = np.array(result, dtype=int) - 127
        return utils.unit_vector(utils.convert_coordinates(result))
    
    def apply_gravity(self, new_coords: tuple, entry_edge: int, possible_angle_range: list):
        """Apply gravity to the current angle for the new state

        Arguments:
            new_coords {tuple} -- Coordinates of the new zone
            entry_edge {int} -- entry edge into the new zone
            possible_angle_range {[type]} -- Possible angle range for the transition over entry edge

        Raises:
            ArithmeticError: if there are no possible new angles

        Returns:
            tuple -- (new starting angle interval, current angle interval in the new zone)
        """
        if self.no_gravity:
            log.debug("---Skipping gravity---")
            return None, None


        log.debug("")
        log.debug("---Applying gravity---")
        # if math.dist(self.turn.coords, self.coords) < 3:
        #     combined_angle_range = ranges_overlap([self.ranges[0][0], self.ranges[1][0]], possible_angle_range)
        #     new_r[0][0] = combined_angle_range[0]
        #     new_r[1][0] = combined_angle_range[1]
        #     return new_r, combined_angle_range
        
        # log.debug("Naive Gravity")
        # result_angles = []
        # for angle in combined_angle_range:
        #     angle = angle + 360 if angle < 90 else angle
        #     distance_to_270 = 270 - angle
        #     # angle += 0.05 * distance_to_270
        #     angle += 3 if distance_to_270 > 0 else -3
        #     result_angles.append(bound_angle(angle))
        # log.debug(result_angles)
        # return result_angles, velocity_range

        if self.mt == settings.BOUNCE or self.turn is None:
            edge_point_0 = utils.edge_coordinates(self.coords,self.entry_edge)
        else:
            edge_point_0 = utils.edge_coordinates(self.turn.coords, self.turn.entry_edge)
        edge_point_new = utils.edge_coordinates(new_coords, entry_edge)

        # calculation_points_cur = utils.calculation_points(edge_point_0,edge_point_cur,combined_angle_range)
        calculation_points_new = utils.calculation_points(edge_point_0,edge_point_new,entry_edge)

        log.debug("cur: %s", self)
        log.debug("new: %s %s", new_coords, possible_angle_range)
        log.debug("edges: %s %s %s %d", edge_point_0, edge_point_new, entry_edge)
        log.debug("delta: %s", calculation_points_new)
        
        ranges = []

        for is_high in [False, True]:

            try:
                # Angles to reach the edge
                angles = utils.angles_to_hit_edge(self.velocity,calculation_points_new,is_high)
                
                angles = utils.interval_intersection(angles, self.theta_range)

                if not self.on_parabola(angles,np.repeat(self.velocity,2)):
                    raise ArithmeticError("angles {} are not on parabola".format(angles))

                # Calculate the current angles for the given starting angles
                current_angles = utils.current_angle(angles, self.velocity, calculation_points_new)


                # Overlap the angle ranges to see if the current range is possible and/or needs to be improved
                combined_angle_range = utils.interval_intersection(possible_angle_range, current_angles)

                # Recalculate starting angles and starting velocities if the angle has changed
                for i in range(len(angles)):
                    if not math.isclose(combined_angle_range[i], current_angles[i], abs_tol=1e-7):
                        starting_angle = utils.start_angle(calculation_points_new,[combined_angle_range[i]], velocity=self.velocity)[0]
                        if utils.in_angle_interval(starting_angle,angles):
                            angles[i] = starting_angle
                ranges.append(angles)
            except (ValueError, ArithmeticError, ZeroDivisionError) as e:
                # log.exception(e)
                continue

        if not ranges:
            raise ArithmeticError("No possible angles")

        list_of_angles = []
        for x in ranges:
            list_of_angles.extend(x)
        
        new_r = utils.edge_angles(list_of_angles)
        current_angles = utils.current_angle(new_r, self.velocity, calculation_points_new)

        log.debug("%s %s", new_r, current_angles)
        return new_r, current_angles

    def on_parabola(self, angles: list, velocity: float):
        """Calculate if the previous state are on the parabola

        Arguments:
            angles {list} -- starting angles to test
            velocity {float} -- starting velocity

        Returns:
            bool -- if the previous states are on the parabola
        """
        s = self
        if self.mt == settings.BOUNCE or self.turn is None:
            return True
        turn_edge = utils.edge_coordinates(self.turn.coords, self.turn.entry_edge)
        try:
            i = 0
            while not s.turn is None and not s is s.turn and s != None and i < settings.consistency_check_length:
                i += 1
                relative_points = utils.calculation_points(turn_edge, utils.edge_coordinates(s.coords, s.entry_edge), s.entry_edge)
                for angle, velocity in zip(angles, velocities):
                    if not utils.hits_edge(angle, velocity, relative_points):
                        return False
                s = s.prev
        except (ValueError, ZeroDivisionError) as e:
            log.exception("Points are not on the parabola",e)
            return False
        return True

    def __str__(self):
        return "("+",".join([str(self.coords), str(None if self.turn is None else self.turn.coords), str(self.entry_edge), str(self.mt), str(self.theta_range), str(self.velocity)])+")"

    def __eq__(self, value):
        if isinstance(value, State):
            result = self.coords == value.coords and \
                    self.turn.coords == value.turn.coords and \
                    self.entry_edge == value.entry_edge and \
                    self.mt == value.mt
            if self.theta_range != None and value.theta_range != None:
                result *= np.allclose(self.theta_range, value.theta_range)
            if self.velocity != None and value.velocity != None:
                result *= np.allclose(self.velocity, value.velocity)
            return result
        else:
            return False

    def show(self):
        """Show the State and its previous states in a matplotlib window

        will not be executed if settings.image_debug is false
        """
        if not settings.image_debug:
            return
        coords = []
        s = self
        # In Debug Mode this is needed for matplotlib to close properly
        fig = plt.figure()
        turn_edge_point = utils.edge_coordinates(s.turn.coords, s.turn.entry_edge)
        while s.prev != None:
            edge_point = utils.edge_coordinates(s.coords, s.entry_edge)
            point = [edge_point[0],-edge_point[1]]
            calc_points = utils.calculation_points(turn_edge_point, edge_point, s.entry_edge)
            coords.append(point)
            try:
                for a in utils.current_angle(s.theta_range,calc_points) :
                    v_point = np.add(utils.direction(a),point)
                    line = np.transpose([v_point,point])
                    plt.plot(line[0],line[1])
            except Exception as e:
                # print(e)
                # import traceback
                # traceback.print_tb(e.__traceback__)
                pass
            if s.turn != s.prev.turn:
                turn_edge_point = utils.edge_coordinates(s.prev.turn.coords, s.prev.turn.entry_edge)
            s = s.prev
        turn_edge_point = utils.edge_coordinates(self.turn.coords, self.turn.entry_edge)
        
        coords = np.transpose(coords)
        plt.plot(coords[0],coords[1])

        # Plot parabola
        if self.theta_range != None:
            x_range = [min([self.coords[0], self.turn.coords[0]]), max([self.coords[0], self.turn.coords[0]])]
            for a in self.theta_range:
                parabola = [[x,utils.projectile_trajectory(x,math.radians(a),self.velocity,O=[turn_edge_point[0],-turn_edge_point[1]])] for x in range(x_range[0],x_range[1],1)]
                parabola = np.transpose(parabola)
                if len(parabola) > 0:
                    plt.plot(parabola[0], parabola[1], label="parabola")
        
        # Plot image boundary

        x_s = [0, settings.img_width, settings.img_width, 0, 0]
        y_s = [0, 0, -settings.img_height, -settings.img_height, 0]
        plt.plot(x_s, y_s)

        if settings.save_images:
            #TODO should the plots be saved?
            pass
        else:
            plt.show()

    def plot_points(self, plotter):
        """Plot previous state history as points to plotter

        Arguments:
            plotter {axis or matplotlib} -- Where to plot the points to
        """
        if not settings.image_debug:
            return
        coords = []
        s = self
        # In Debug Mode this is needed for matplotlib to close properly
        while s.prev != None:
            edge_point = utils.edge_coordinates(s.coords, s.entry_edge)
            point = [edge_point[0],-edge_point[1]]
            coords.append(point)
            s = s.prev
        coords = np.transpose(coords)
        plotter.plot(coords[0],coords[1])


    def show_on_image(self, img, color_prev=(None,None,255), color_to_show=(None,None,100), image_name="tmp.png"):
        """Reveal the state history on an image

        Arguments:
            img {Image} -- Image to draw the state history to

        Keyword Arguments:
            color_prev {tuple} -- color to restore at the respective pixels (default: {(None,None,255)})
            color_to_show {tuple} -- color to print at the pixels (default: {(None,None,100)})
            image_name {str} -- file to save the image to if settings.save_images is true (default: {"tmp.png"})
        """
        if not settings.image_debug:
            return
        prev = self
        while prev != None:

            utils.put_pixel_save(img, prev.coords, color_to_show)
            prev = prev.prev
        if settings.save_images:
            img.save(image_name)
        else:
            img.show()
        prev = self
        while prev != None:
            utils.put_pixel_save(img, prev.coords, color_prev)
            prev = prev.prev

    def path_length(self):
        """Primitive path length of the state history. Only considers number of states

        Returns:
            int -- length of the path
        """
        length = 0
        s = self
        while s != None:
            length += settings.edge_length
            s = s.prev
        return length

    def better_path_length(self):
        """Path length for parabolas. Calculates the distance between turn points

        Returns:
            float -- length of the path
        """
        length = 0
        s = self
        if self.no_gravity:
            return self.path_length()
        else:
            while s.turn is not None:
                length += math.dist(s.coords, s.turn.coords)
                s = s.turn
        return length
    
    def actual_path_length(self):
        """Path length that is more accurate to the acutal path. uses the distance between edges

        Returns:
            float -- length of the path
        """
        s = self
        length = 0
        while s is not None:
            if s.prev is not None and (s.prev.entry_edge % 2) != (s.entry_edge % 2):
                length += settings.diagonal_edge_to_edge
                s = s.prev
            else:
                length += settings.edge_length
                s = s.prev
        return length
    
    def better_actual_path_length(self):
        """Combination of parabola path length and actual path length if no_gravity is true

        Returns:
            float -- length of the path
        """
        s = self
        length = 0
        if self.no_gravity:
            length = self.actual_path_length()
        else:
            length = self.better_path_length()
        return length

    def start_angle_range(self):
        """Return the angle interval before the last turn point

        Returns:
            list -- angle interval
        """
        first_turn = self.first_turn()
        if first_turn is not None:
            if first_turn.prev is not None:
                return first_turn.prev.theta_range
            else:
                return self.theta_range
        else:
            return self.theta_range

    def first_turn(self):
        """Return the first turn point

        Returns:
            State -- first turn point
        """
        cur_turn = self.turn
        prev_turn = cur_turn

        while cur_turn is not None and cur_turn.turn is not None:
            prev_turn = cur_turn
            cur_turn = cur_turn.turn
        return prev_turn

    def origin(self):
        """Return the origin of the simulation

        Returns:
            State -- origin state
        """
        cur_turn = self.turn

        while cur_turn.prev is not None:
            cur_turn = cur_turn.turn
        return cur_turn
