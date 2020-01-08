import numpy as np
import pandas as pd
import math

from sklearn import metrics

__all__ = ["evaluate_prediction_table", "evaluate_prediction_metrics", "evaluate_prediction_classifier", "calculate_information_values"]

""" Comparison of classifier performance.

:param scores   ->  classifier predictions
"""
def evaluate_classifiers(scores):
    # MODEl: testing (original labels) // decision tree // random forest // logistic regression
    evals = [[0,0,0],[0,0,0],[0,0,0],[0,0,0]]
    for level in range(len(scores)):
        for model in range(len(scores[level])):
            if (scores[level][0] == scores[level][model]):
                evals[model][0]+=1
            if ((scores[level][0] == 'lost') & (scores[level][model] == 'won')):
                evals[model][1]+=1
            if ((scores[level][0] == 'won') & (scores[level][model] == 'lost')):
                evals[model][2]+=1
    print(evals)


""" Save tupel predictions (won/lost including certainty estimation) to a .csv file.

:param labels_testing   ->  testing labels
:param predictions      ->  tupel predictions
:param filename         -> .csv location
:param debug            -> verbose debugging information
"""
def evaluate_prediction_table(labels_testing, predictions, filename, debug=False):
    if type(predictions) is tuple:
        confidence = [x[1] for x in predictions[1]]
        results = pd.DataFrame({'real': labels_testing, 'predicted': predictions[0], 'probability': confidence})
    else :
        results = pd.DataFrame({'real': labels_testing, 'predicted': predictions})
    results.to_csv(filename)
    if debug:
        print(predictions)
        print(results)


""" Print model metrics to the command line.

:param labels_testing   ->  testing labels
:param predictions      ->  predicted labels
"""
def evaluate_prediction_metrics(labels_testing, predictions, opt):
    mean_absolute_error = metrics.mean_absolute_error(labels_testing, predictions)

    mean_achieved_score = labels_testing.mean(axis=0)
    print("Mean achieved score:",mean_achieved_score)
    print("Mean absolute error:",mean_absolute_error)
    error_ratio = mean_absolute_error / mean_achieved_score
    print("Mean absolute error makes up",error_ratio,"of the avg achieved score.")
    print("Sqrt of mean squared error:",math.sqrt(metrics.mean_squared_error(labels_testing,predictions)))
    print("Mean squared log error:",math.sqrt(metrics.mean_squared_log_error(labels_testing,predictions)))
    print("R^2 score:",metrics.r2_score(labels_testing,predictions))


""" Print small classifier evaluation to the command line.

:param labels_testing   ->  testing labels
:param predictions      ->  predicted labels
"""
def evaluate_prediction_classifier(labels_testing, predictions):
    if type(predictions) is tuple:
        probability = [x[1] for x in predictions[1]]
        predictions = [x for x in predictions[0]]
        print("Brier score loss:", metrics.brier_score_loss(labels_testing.values, probability))
        print(metrics.classification_report(labels_testing.values, predictions))
    else :
        print(metrics.classification_report(labels_testing.values, predictions))


""" Calculate and visualize information values for the feature input.

:param all_features         ->  all known features
:param considered_features  ->  features in use
:param target_label         ->  feature discriminability with respect to a target label
:param debug                ->  verbose debugging information

:return numerical   vector describing the collocation of identified strategies per played level
"""
def calculate_information_values(df, all_features, considered_features, target_label, debug=False):
    result = dict(zip(all_features, [0 for i in range(len(all_features))]))
    for feature in considered_features:
        lst = []
        for i in range(df[feature].nunique()):
            val = list(df[feature].unique())[i]
            lst.append([
                feature,
                val,
                df[df[feature] == val].count()[feature],
                df[(df[feature] == val) & (df[target_label] == 'WON')].count()[feature],
                df[(df[feature] == val) & (df[target_label] == 'LOST')].count()[feature]
            ])
        data = pd.DataFrame(lst, columns=['Variable','Value','All','Good','Bad'])
        data['Share'] = data['All'] / data['All'].sum() # not used currently
        data['Bad Rate'] = data['Bad'] / data['All']
        #data['Distribution Good'] = (data['All'] - data['Bad']) / (data['All'].sum() - data['Bad'].sum())
        data['Distribution Good'] = (data['All'] - data['Bad']) / data['All'].sum()
        #data['Distribution Bad'] = data['Bad'] / data['Bad'].sum()
        data['Distribution Bad'] = data['Bad'] / data['All'].sum()
        data['WoE'] = np.log(data['Distribution Good'] / data['Distribution Bad'])
        data = data.replace({'WoE': {np.inf: 0, -np.inf: 0}})

        data['IV'] = data['WoE'] * (data['Distribution Good'] - data['Distribution Bad'])

        data = data.sort_values(by=['Variable', 'Value'], ascending=[True, True])
        data.index = range(len(data.index))

        if debug:
            print(data)
            print('IV = ', data['IV'].sum())

        result[feature] = (data['IV'].sum(), data['WoE'].sum())
    return result
