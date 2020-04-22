#!/usr/bin/env python3

import collections
import copy
import csv
import math
import os
import random
import time
from enum import Enum

import numpy as np
import pandas as pd
from sklearn import metrics

from . import query

__current_working__dir = os.path.dirname(os.path.abspath(__file__))

demo_level = []
demo_round = (0, "LOST")


class Algorithm(Enum):
    GREEDY = 'Greedy'
    EPSILON_GREEDY = 'Epsilon Greedy'
    ADAPTIVE_EPSILON = 'Adaptive Epsilon'


def initialize_demo_matrix(num_levels, num_rounds):
    """
    Initialize the game matrix with dummy data

    Parameters
    ----------
    num_levels : int
        Number of levels
    num_rounds : int
        Number of rounds

    Returns
    -------
    dict
        Dictionary with a list of dummy game outcomes (0, 'LOST') for each level
    """
    num_rounds = int(num_rounds)
    global demo_level
    demo_level = [demo_round for i in range(num_rounds)]
    matrix = dict([(i, demo_level.copy())
                   for i in range(1, num_levels + 1)])
    return matrix


def initialize_game_matrix(filename, num_levels=21):
    """
    Initialize the game matrix with data from a loaded csvfile

    Parameters
    ----------
    csvfile : string
        File with features for all levels at least 'levelid', 'num_times_played', 'game_won' and 'num_score'
    num_levels : int, optional
        Number of Levels (default is 21)

    Returns
    -------
    dict
        Dictionary with a list of game outcomes i.e. (34829, 'WON') for each level, Scores are the improvement depending on the max-score
    """
    max_scores = {}
    with open(filename, 'r') as csvfile:
        csvfile.seek(0)
        reader = csv.DictReader(csvfile, delimiter=",", quotechar="|")
        row_count = len(list(reader))
        game_matrix = initialize_demo_matrix(
            num_levels=num_levels, num_rounds=1 + row_count/num_levels)
        # Reset Reader
        csvfile.seek(0)
        reader = csv.DictReader(csvfile, delimiter=",", quotechar="|")
        left_levels = [i+1 for i in range(num_levels)]
        for row in reader:
            level_id = int(row['levelid'])
            if level_id in range(1, num_levels+1):
                if int(row['num_times_played']) == 0:
                    left_levels.remove(level_id)
                    if row['game_won'] == 'WON':
                        max_scores[level_id] = int(row['num_score'])
                    else:
                        max_scores[level_id] = 0
                else:
                    if row['game_won'] == 'WON':
                        game_matrix[level_id][int(row['num_times_played'])-1] = (
                            int(row['num_score']), row['game_won'])
                    else:
                        game_matrix[level_id][int(
                            row['num_times_played'])-1] = (0, row['game_won'])

        for level_id in left_levels:
            max_scores[level_id] = 0

    return max_scores, game_matrix


def calculate_max_score(max_attempts, game_matrix, max_scores):
    """
    Calculate the maximal possible Score (currently not regarding the order of playing)

    Parameters
    ----------
    max_attempts : int
        Number of attempts
    game_matrix : dict
        Dictionary with a list of game outcomes i.e. (34829, 'WON') for each level

    Returns
    -------
    int
        Maximum possible score
    """
    for lvl in game_matrix.keys():
        game_matrix[lvl].sort(reverse=True)
    for i in range(0, max_attempts):
        chosen_level = demo_level
        chosen_level_id = 0
        chosen_round = demo_round
        for lvl in game_matrix.keys():
            if game_matrix[lvl][0][1] == 'WON' and game_matrix[lvl][0][0] > chosen_round[0]:
                chosen_round = game_matrix[lvl][0]
                chosen_level = game_matrix[lvl]
                chosen_level_id = lvl

        if chosen_round == demo_round or chosen_level == demo_level:
            break
        max_scores[chosen_level_id] = max(chosen_round[0],max_scores[chosen_level_id])
        chosen_level.remove(chosen_round)
    return sum(max_scores.values())


class Level:
    """
    A class used to simulate the behaviour of Java-Class meta.Level

    Attributes
    ----------
    last_score : int
        Last score of the level
    max_score : int
        Maximum achieved score of the level
    features : dict
        The levels features i.e. 'num_birds'

    Methods
    -------
    getBestScore()
        Returns the current best score
    getEstimatedMaximalPoints()
        Returns the estimated maximal possible points
    calculate_maximal_points()
        Calculates the estimated maximal possible points
    play(score)
        Stores the new Score in the attributes
    """

    def __init__(self, features, last_score=0, max_score=0, *args, **kwargs):
        """
        Parameters
        ----------
        last_score : int
            Last score of the level
        max_score : int
            Maximum achieved score of the level
        features : dict
            The levels features i.e. 'num_birds'
        """
        self.last_score = last_score
        self.max_score = max_score
        self.features = features
        self.levelId = features['levelid']
        self.maximalPointsWithoutPigs = self.calculate_maximal_points()
        self.numberOfTimesPlayed = 1
        return super().__init__(*args, **kwargs)

    def getBestScore(self):
        return self.max_score

    def getEstimatedMaximalPoints(self):
        return self.maximalPointsWithoutPigs + (int(self.features['num_birds']) - 1) * 10000

    def calculate_maximal_points(self):
        result = int(self.features['num_pigs']) * 5300
        result += int(self.features['num_destroyable_objects']) * 700
        return result

    def play(self, score):
        """
        Parameters
        ----------
        score : int
            The new score
        """
        self.last_score = score
        self.max_score = max(self.max_score, score)
        self.numberOfTimesPlayed += 1


def old_level_selection(game_matrix, filename, num_levels, max_attempts, max_scores):
    """
    Simulate the old Level Selection (Port from level_selection.Action)

    Parameters
    ----------
    num_levels : int
        Number of Levels
    max_scores : dict
        Dictionary with a score for each level
    max_attempts : int
        Number of attempts
    featureList_file : TextIOWrapper
        File with level features
    game_matrix : dict
        Dictionary with a list of game outcomes i.e. (34829, 'WON') for each level

    Returns
    -------
    int
        Achieved score
    """
    
    score = 0
    features_dict = {}
    
    with open(filename, 'r') as featureList_file:
        
        features_list_reader = csv.DictReader(
            featureList_file, delimiter=",", quotechar="|")
        features_dummy = next(features_list_reader)
        for feature in list(features_dummy):
            if feature == 'game_won':
                features_dummy[feature] = 'LOST'
            elif feature == 'list_strategies':
                features_dummy[feature] = np.NaN
            else:
                features_dummy[feature] = 0

        featureList_file.seek(0)
        features_list_reader = csv.DictReader(
            featureList_file, delimiter=",", quotechar="|")

        left_levels = [i+1 for i in range(num_levels)]
        while len(left_levels) > 0:
            try:
                features = next(features_list_reader)
            except StopIteration as stop:
                break
            level_id = int(features['levelid'])
            if level_id in left_levels:
                features_dict[level_id] = features
                left_levels.remove(level_id)
        for level_id in left_levels:
            features = copy.deepcopy(features_dummy)
            features['levelid'] = level_id
            features_dict[level_id] = features

        levelStorage = dict([(i, Level(dict(features_dict[i]), max_scores[i],
                                    max_scores[i])) for i in range(1, num_levels+1)])

        currentLevel = -1
        startLevel = 1
        numberOfTimesPlayedTheMost = -1
        numberOfTimesPlayedMinimum = 1000
        allLostLevelsPlayedTwice = False
        lastLostLevel = np.argmin(list(max_scores.values())) + 1

        allLevelsPlayedOnce = True

        currentIteration = num_levels

        for i in range(max_attempts):
            probs = dict.fromkeys(max_scores.keys(), 0)
            bestLevelIndex = -1
            bestProp = -10000
            containsLost = np.min(list(max_scores.values())) == 0
            lostCounter = 0

            lostLevel = levelStorage.get(lastLostLevel, None)
            if lastLostLevel > 0 and lostLevel.getBestScore() != 0:
                allLostLevelsPlayedTwice = True
            else:
                lastLostLevel = -1

            for lvl in max_scores.keys():
                level = levelStorage[lvl]

                if numberOfTimesPlayedTheMost < level.numberOfTimesPlayed:
                    numberOfTimesPlayedTheMost = level.numberOfTimesPlayed

                if numberOfTimesPlayedMinimum > level.numberOfTimesPlayed:
                    numberOfTimesPlayedMinimum = level.numberOfTimesPlayed

                eMaxPoints = level.getEstimatedMaximalPoints()
                bestScore = level.getBestScore()

                probability = 1 - (bestScore / eMaxPoints)
                if bestScore > eMaxPoints:
                    probs[lvl] = 0
                else:
                    probs[lvl] = probability

                if probs[lvl] == 1:
                    containsLost = True
                    lostCounter += 1
                    lastLostLevel = lvl

                # print("Prob for Level " + str(level.levelId) + " " + str(probability) +
                #       ", Played: " + str(level.numberOfTimesPlayed) + " times")

            for lvl in max_scores.keys():
                level = levelStorage[lvl]

                # First redo all lost levels at most 2 times
                if (not allLostLevelsPlayedTwice) and containsLost and probs[lvl] == 1 and numberOfTimesPlayedTheMost <= 3\
                        and level.numberOfTimesPlayed < 3:
                    bestLevelIndex = lvl
                    bestProp = probs[lvl]

                    if lvl == lastLostLevel and level.numberOfTimesPlayed == 2:
                        allLostLevelsPlayedTwice = True
                    break

                # if all lost levels were played twice and there are still some lost levels
                if allLostLevelsPlayedTwice and containsLost:
                    # if more than 15% of the total number of levels are lost levels, try it again
                    # only one more time
                    if lostCounter > 0.15 * num_levels and probs[lvl] == 1 and level.numberOfTimesPlayed == 3:
                        bestLevelIndex = lvl
                        bestProp = probs[lvl]
                        # print(
                        #     "[Meta] All lost levels have been played at most twice but there are still lost levels remaining.")
                        # print(
                        #     "[Meta] Too many lost levels left so try one last lost level again.")
                        break
                        # else give up on these levels
                    elif lostCounter < 0.15 * num_levels or numberOfTimesPlayedTheMost >= 4:
                        containsLost = False
                        # print(
                        #     "[Meta] All lost levels have been played at most twice but there are still lost levels remaining.")
                        # print("[Meta] Give up on the lost levels.")

                # If there are no lost levels anymore
                # ignore the lost levels
                # Choose the level with the highest probability
                if not containsLost and probs[lvl] != 1 and probs[lvl] > bestProp\
                        and level.numberOfTimesPlayed < numberOfTimesPlayedMinimum + 2:
                    bestLevelIndex = lvl
                    bestProp = probs[lvl]

            if bestProp == -10000:
                # print("Something went wrong ... Choosing random level...")
                bestLevelIndex = random.randint(1, num_levels)

            currentLevel = bestLevelIndex

            # print("Selecting Level " + str(currentLevel) +
            #       " Probability: " + str(bestProp * 100) + "%")

            score_result = game_matrix[currentLevel].pop(
                0) if game_matrix[currentLevel] else demo_round
            if score_result[1] == 'WON':
                levelStorage[currentLevel].play(score_result[0])
                max_scores[currentLevel] = max(score_result[0],max_scores[currentLevel])
            else:
                levelStorage[currentLevel].play(0)
        # print('Achieved scores (Old):\n\t'+str(max_scores))
    return sum(max_scores.values())


def simulate_level_selection(game_matrix, filename, num_levels, max_attempts, max_scores, epsilon=0.1, algorithm=Algorithm.GREEDY, enable_softmax=False, classifier_type='tree', regressor_type="tree", verbose=False):
    """
    Simulate the new Level Selections (Implemented in level_selection.Action)

    Parameters
    ----------
    num_levels : int
        Number of Levels
    max_scores : dict
        Dictionary with a score for each level
    max_attempts : int
        Number of attempts
    filename : string
        File with level features
    game_matrix : dict
        Dictionary with a list of game outcomes i.e. (34829, 'WON') for each level
    epsilon: float, optional
        Initial epsilon value for Epsilon Algorithms (default is 0.1)
    algorithm: Algorithm, optional
        The algorithm used for deciding which level to play next (default is Greedy)
    enable_softmax: bool, optional
        If disabled choose a level randomly if algorithm decides on Exploration. (default)
        Otherwise the Decision is made based on the improvement predictions probability distribution
    regressor_type : string, optional
        Options: 'tree' (default), 'linear', 'forest'
    classifier_type : string, optional
        Options: 'tree' (default), 'logistic', 'forest'
    verbose: bool, optional
        enable debug output, default False

    Returns
    -------
    int
        Achieved score
    """
    features_list = []
    
    with open(filename, 'r') as featureList_file:
        featureList_file.seek(0)
        features_list_reader = csv.DictReader(
            featureList_file, delimiter=",", quotechar="|")
        features_dummy = next(features_list_reader)
        for feature in list(features_dummy):
            if feature == 'game_won':
                features_dummy[feature] = 'LOST'
            elif feature == 'list_strategies':
                features_dummy[feature] = np.NaN
            else:
                features_dummy[feature] = 0

        featureList_file.seek(0)
        features_list_reader = csv.DictReader(
            featureList_file, delimiter=",", quotechar="|")
        number_of_times_lost = 0
        elapsed_times = []
        epsilon_init = epsilon
        if algorithm == Algorithm.ADAPTIVE_EPSILON:
            epsilon = 0
        if epsilon < 0.0 or epsilon > 1.0:
            raise ValueError('Epsilon needs to be in range of 0 to 1')

        left_levels = [i+1 for i in range(num_levels)]
        while len(left_levels) > 0:
            try:
                features = next(features_list_reader)
            except StopIteration as stop:
                break
            level_id = int(features['levelid'])
            if level_id in left_levels:
                features_list.insert(level_id-1, features)
                left_levels.remove(level_id)
        for level_id in left_levels:
            features = copy.deepcopy(features_dummy)
            features['levelid'] = level_id
            features_list.insert(level_id-1, features)
        data = pd.DataFrame(features_list)
        for lvl in max_scores.keys():
            data.at[lvl-1, 'max_score'] = max_scores[lvl]
            data.at[lvl-1, 'num_times_played'] = 1

        for i in range(max_attempts):
            start = time.time()

            # request predictions from the decision tree
            pred_class, pred_reg = query.query_predictions(
                data, classifier_type, regressor_type)

            win_pred_error = metrics.brier_score_loss(
                [1 if x > 0 else 0 for x in max_scores], pred_class[1].take(1, 1))

            # TODO: Keep or not?
            number_of_won_levels = 0
            score_squared_error = 0
            score_sum = 0
            for levelId, max_score in max_scores.items():
                pred_reg[levelId-1] = max(pred_reg[levelId-1], 0)
                if max_score > 0:
                    number_of_won_levels += 1
                    score_sum += max_score
                    score_squared_error += (max_score -
                                            pred_reg[levelId-1])**2
            if score_sum == 0:
                score_error = 1
            else:
                score_error = math.sqrt(score_squared_error /
                                        number_of_won_levels) / (score_sum/number_of_won_levels)

            # abs_error = math.sqrt(metrics.mean_squared_error(
            #     list(max_scores.values()), pred_reg))
            # abs_error = metrics.mean_absolute_error(
            #     list(max_scores.values()), pred_reg)
            # score_error = abs_error / np.mean(pred_reg)

            # Make our scores represent the improvement and adapt them to
            improvement_predictions = copy.deepcopy(pred_reg)

            for levelId, max_score in max_scores.items():
                improvement = improvement_predictions[levelId - 1] - max_score
                improvement_with_won = improvement_predictions[levelId -
                                                            1] * pred_class[1][levelId-1][1] - max_score

                pred_score = improvement * win_pred_error

                ## improvement * win probability * score error ##
                # pred_score += improvement * \
                #     pred_class[1][levelId-1][1] * score_error

                ## improvement_with_won * score error ##
                pred_score += improvement_with_won * score_error

                pred_score /= win_pred_error + score_error
                improvement_predictions[levelId-1] = pred_score

            # Calculate epsilon as the error ratio between the current scores and our predictions
            if algorithm is Algorithm.ADAPTIVE_EPSILON:
                mean_error = ((score_error+win_pred_error) / 2)
                # epsilon = min(mean_error + (max_attempts - i)/(max_attempts+num_levels), 1)
                epsilon = min(mean_error, 1)
                print('ɛ_ad:',epsilon, 's_e:',score_error,'w_e:',win_pred_error) if verbose else ''
                
            # In probability of epsilon select another random level
            if algorithm == Algorithm.GREEDY or random.choices([True, False], weights=[1-epsilon, epsilon])[0]:
                # select the level with highest improvement
                selected_level = random.randint(1, num_levels)
                max_level = improvement_predictions.argmax() + 1
                if improvement_predictions[max_level-1] != 0:
                    selected_level = max_level
            else:
                if enable_softmax:
                    # Map the improvement values to probabilities with softmax algorithm and choose one randomly
                    probabilities = softmax(improvement_predictions)
                    selected_level = random.choices(
                        range(1, num_levels+1), weights=probabilities)[0]
                else:
                    selected_level = random.randint(1, num_levels)

            score_result = game_matrix[selected_level].pop(
                0) if game_matrix[selected_level] else demo_round

            data.at[selected_level-1, 'num_times_played'] += 1
            if score_result[1] == 'WON':
                max_scores[selected_level] = max(score_result[0], max_scores[selected_level])
                data.at[selected_level-1, "max_score"] = score_result[0]
            #     epsilon = max(epsilon-0.2, epsilon_init)
            else:
            #     epsilon = min(epsilon+0.05, 1)
                number_of_times_lost += 1
            elapsed_times.append(time.time() - start)
        if verbose:
            print('Achieved scores:\n\t'+str(max_scores))
            print('Lost racio:', number_of_times_lost/max_attempts)
            print("Average time for one level selection ("+algorithm.value+" with classifier="+classifier_type+", regressor="+regressor_type+", softmax=" +
                str(enable_softmax)+"):\n\t"+str(sum(elapsed_times)/float(len(elapsed_times))*1000) + " ms")
        return sum(max_scores.values())


def softmax(values, theta=1.0, axis=None):
    """
    Source: https://nolanbconaway.github.io/blog/2017/softmax-numpy
    Compute the softmax of each element along an axis of X.

    Parameters
    ----------
    X: ND-Array. Probably should be floats.
    theta (optional): float parameter, used as a multiplier
        prior to exponentiation. Default = 1.0
    axis (optional): axis to compute values along. Default is the
        first non-singleton axis.

    Returns an array the same size as X. The result will sum to 1
    along the specified axis.
    """

    # make our score values smaller
    mean_value = np.mean(np.abs(values))
    for i in range(len(values)):
        values[i] = values[i] / mean_value
    # make X at least 2d
    y = np.atleast_2d(values)

    # find axis
    if axis is None:
        axis = next(j[0] for j in enumerate(y.shape) if j[1] > 1)

    # multiply y against the theta parameter,
    y = y * float(theta)

    # subtract the max for numerical stability
    y = y - np.expand_dims(np.max(y, axis=axis), axis)

    # exponentiate y
    y = np.exp(y)

    # take the sum along the specified axis
    ax_sum = np.expand_dims(np.sum(y, axis=axis), axis)

    # finally: divide elementwise
    p = y / ax_sum

    # flatten if X was 1D
    if len(values.shape) == 1:
        p = p.flatten()

    return p
