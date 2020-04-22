import math
import pandas as pd
import os

__all__ = ["read_input", "numerical_transform_strategies", "get_weighted_strategies"]

"""" Main function for construction and evaluation of the regression based decision tree.

:param input_folder ->  folder with feature input
:param debug        ->  verbose outputs for debugging purposes

:return data        ->  features for training / testing
"""
def read_input(input_folder, debug):
    # limit number of input lines with nrows (avoids buggy feature vectors)
    # data = pd.read_csv(input_folder)

    data = pd.concat([pd.read_csv(os.path.join(input_folder, file)) for file in os.listdir(input_folder) if file.endswith('.csv')], ignore_index=True, sort=True)

    # insert base case (if Level was not correctly recognized)
    # features_dummy = data.iloc[0].to_numpy()
    features_dummy = {}
    for feature in data.columns:
        if feature == 'game_won':
            features_dummy[feature] = 'LOST'
        elif feature == 'list_strategies':
            features_dummy[feature] = math.nan
        else:
            features_dummy[feature] = 0
    features_dummys = [features_dummy.copy() for i in range(10)]
    data = data.append(pd.DataFrame(features_dummys), ignore_index=True)
    # number of recorded instances x features
    print("found " + str(data.shape[0]) + " recordings over " + str(data.shape[1]) + " features.")
    # inspect input data
    if debug:
        print(data.head(10))

    return data


""" Transforms the 'list_strategies' column from the feature input into a numerical vector suited for decision trees.

:param data         ->  csv feature input
:param strategies   ->  list of known strategies

:return             ->  numerical vector describing the collocation of identified strategies per played level
"""
def numerical_transform_strategies(data, strategies, strategies_weights ):
    width = len(data['list_strategies'])
    height = len(strategies)
    matrix_strategies = [[0 for x in range(height)] for y in range(width)]

    for playedLevel in range(width):
        if isinstance(data['list_strategies'][playedLevel], float) and math.isnan(data['list_strategies'][playedLevel]):
            continue
        for strategy in range(height):
            # print('Trying index: ', playedLevel, ' with strategy: ', strategy)
            matrix_strategies[playedLevel][strategy] = data['list_strategies'][playedLevel].count(strategies[strategy])

    sum = 0
    vector_strategies = [0 for x in range(width)]
    for playedLevel in range(len(matrix_strategies)):
        for strategy in range(len(strategies)):
            sum = sum + matrix_strategies[playedLevel][strategy]*strategies_weights[strategy]
        vector_strategies[playedLevel] = sum
        sum = 0

    assert len(vector_strategies) == len(data['list_strategies']), "Numerical transformation of strategies failed."
    return vector_strategies


""" Anchor for known strategies and their statically assigned weights.

:return ->  list of known strategies and their respective weights
"""
def get_weighted_strategies():
    strategies = ('bunker', 'domino', 'collapseStructure', 'heavyObject', 'targetPig', 'defrost')  # known strategies
    strategies_weights = (1, 2, 4, 10, 5, 15)
    return strategies, strategies_weights