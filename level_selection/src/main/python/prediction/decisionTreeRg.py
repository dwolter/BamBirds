import pickle
import os

import sklearn.tree as sklTree

from sklearn2pmml import sklearn2pmml
from sklearn2pmml.pipeline import PMMLPipeline

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
    pipeline = PMMLPipeline([("regressor", sklTree.DecisionTreeRegressor(criterion="friedman_mse", splitter='best', max_depth=5))])
    pipeline.fit(features_training, labels_training)
    
    with open(tr_regressor_output, 'wb') as file:
        pickle.dump(pipeline, file)
    return pipeline