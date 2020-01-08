import pickle
import os

import sklearn.tree as sklTree

__all__ = ["train_regressor"]

__current_working__dir = os.path.dirname(os.path.abspath(__file__))
tr_regressor_output = os.path.join(__current_working__dir,'tr_regressor.pkl')


""" Trains a decision tree regressor and pickles the final model.

:param features_training    ->  training features
:param labels_training      ->  training labels
:param debug                ->  verbose debugging information

:return                     ->  regressor model
"""
def train_regressor(features_training, labels_training, debug= False):
    regressor = sklTree.DecisionTreeRegressor(criterion="friedman_mse", splitter='best', max_depth=5)
    regressor.fit(features_training, labels_training)
    if debug: print(regressor.feature_importances_)
    with open(tr_regressor_output, 'wb') as file:
        pickle.dump(regressor, file)
    return regressor