import pickle
import os

from . import learning_dataPrep as dp

__all__ = ["query_predictions", "query_classifier", "query_regressor"]

__dir_path__ = os.path.dirname(os.path.abspath(__file__))
tr_classifier_output = os.path.join(__dir_path__,'tr_classifier.pkl')
rf_classifier_output = os.path.join(__dir_path__,'rf_classifier.pkl')
lr_classifier_output = os.path.join(__dir_path__,'lr_classifier.pkl')
tr_regressor_output = os.path.join(__dir_path__,'tr_regressor.pkl')
lm_regressor_output = os.path.join(__dir_path__,'lm_regressor.pkl')
rf_regressor_output = os.path.join(__dir_path__,'rf_regressor.pkl')

tr_cla_ = 'undefined'
rf_cla_ = 'undefined'
lr_cla_ = 'undefined'
tr_reg_ = 'undefined'
lm_reg_ = 'undefined'
rf_reg_ = 'undefined'


""" Allows queries to classifier and regressor models to determine success probability and score estimations.

:param feature_vector   ->  features used for querying the models
:param classifier       ->  instance of a classifier
:param regressor        ->  instance of a regressor

:return                 ->  binary (won/lost) prediction and score estimation
"""
def query_predictions(feature_vector, classifier='tree', regressor='tree'):
    feature_vector = feature_vector.sort_index(1)
    strategies, strategies_weights = dp.get_weighted_strategies()
    numerical_strategies = dp.numerical_transform_strategies(feature_vector, strategies, strategies_weights)

    features_binaryClassifier = feature_vector.drop(['levelid', 'num_score', 'list_strategies', 'game_won'],
                                                    axis=1).copy()
    features_binaryClassifier['numerical_strategies'] = numerical_strategies

    features_regression = feature_vector.drop(['levelid', 'num_score', 'list_strategies', 'game_won'], axis=1).copy()
    features_regression['numerical_strategies'] = numerical_strategies

    global tr_cla_, rf_cla_, lr_cla_, tr_reg_, lm_reg_, rf_reg_

    if classifier == 'tree':
        if tr_cla_ == 'undefined':
            with open(tr_classifier_output, 'rb') as class_file:
                cla = pickle.load(class_file)
                tr_cla_ = cla
        else:
            cla = tr_cla_
    elif classifier == 'forest':
        if rf_cla_ == 'undefined':
            with open(rf_classifier_output, 'rb') as class_file:
                cla = pickle.load(class_file)
                rf_cla_ = cla
        else:
            cla = rf_cla_
    elif classifier == 'logistic':
        if lr_cla_ == 'undefined':
            with open(lr_classifier_output, 'rb') as class_file:
                cla = pickle.load(class_file)
                lr_cla_ = cla
        else:
            cla = lr_cla_

    if regressor == 'tree':
        if tr_reg_ == 'undefined':
            with open(tr_regressor_output, 'rb') as reg_file:
                reg = pickle.load(reg_file)
                tr_reg_ = reg
        else:
            reg = tr_reg_
    elif regressor == 'forest':
        if rf_reg_ == 'undefined':
            with open(rf_regressor_output, 'rb') as rf_file:
                reg = pickle.load(rf_file)
                rf_reg_ = reg
        else:
            reg = rf_reg_
    elif regressor == 'linear':
        if lm_reg_ == 'undefined':
            with open(lm_regressor_output, 'rb') as lm_file:
                reg = pickle.load(lm_file)
                lm_reg_ = reg
        else:
            reg = lm_reg_

    pred_class = query_classifier(features_binaryClassifier, cla)
    pred_reg = query_regressor(features_regression, reg)

    return pred_class, pred_reg


""" Subquery to the classifier.

:param features_testing ->  testing features
:param classifier       ->  instance of a classifier

:return                 ->  won/lost prediction including certainty estimation
"""
def query_classifier(features_testing, classifier):
    labels_predicted_proba = classifier.predict_proba(features_testing)
    labels_predicted = classifier.predict(features_testing)
    return labels_predicted, labels_predicted_proba


""" Subquery to the regressor.

:param features_testing ->  testing features
:param regressor        ->  instance of a regressor

:return                 -> predicted label (score)
"""
def query_regressor(features_testing, regressor):
    labels_predicted = regressor.predict(features_testing)
    return labels_predicted
