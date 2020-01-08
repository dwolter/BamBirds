import pickle
import os

from sklearn.ensemble import RandomForestRegressor

from . import randomForestCl

__all__ = ["random_forest", "train_random_forest", "query_random_forest"]

__current_working__dir = os.path.dirname(os.path.abspath(__file__))
rf_regressor_output = os.path.join(__current_working__dir,'rf_regressor.pkl')
forest_regressor_java = os.path.join(__current_working__dir,'../../Java/src/level_selection/model_representation/RandomForestRegressor.java')


""" Creates a random forest regressor / orchestrates training and evaluation.

:param features_training    ->  training features
:param labels_training      ->  training labels
:param features_testing     ->  testing features
:param labels_testing       ->  testing labels

:return                     ->  predicted labels (scores)
"""
def random_forest(features_training, labels_training, features_testing, labels_testing):
    rfreg = train_random_forest(features_training, labels_training)
    randomForestCl.forest_to_java_code(rfreg, list(features_training.columns), forest_regressor_java, 'RandomForestRegressor')
    labels_predicted = query_random_forest(rfreg, features_testing)
    with open(rf_regressor_output, 'wb') as file:
        pickle.dump(rfreg, file)
    return labels_predicted


""" Trains a random forest regressor and returns its instance.

:param features_training    ->  training features
:param labels_training      ->  training labels

:return                     ->  regressor model
"""
def train_random_forest(features_training, labels_training):
    regressor = RandomForestRegressor(n_estimators=1500, random_state=73, max_depth=12)
    regressor.fit(features_training, labels_training)
    return regressor


""" Queries a random forest regressor.

:param rfreg   ->  instance of a random forest classsifier
:param features_testing ->  testing features

:return                     ->  predicted labels (scores)
"""
def query_random_forest(rfreg, features_testing):
    labels_predicted = rfreg.predict(features_testing)
    return labels_predicted