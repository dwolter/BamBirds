import matplotlib.pyplot as plt
import pandas as pd
import pickle
import os

import sklearn.tree as sklTree
import sklearn.model_selection as sklModel
from sklearn.linear_model import LogisticRegression

from . import learning_dataPrep as dataPrep

from . import decisionTreeCl
from . import decisionTreeRg

from . import randomForestCl
from . import randomForestRg

from . import linearModel

from . import query
from . import evaluate

__current_working__dir = os.path.dirname(os.path.abspath(__file__))
feature_input = os.path.join(__current_working__dir,"featureList.csv")
learning_folder = os.path.join(__current_working__dir,"../../../data/learning")
testing_folder = os.path.join(__current_working__dir,"../../../data/testing")
binary_prediction_output = os.path.join(__current_working__dir,"predictions/binary_prediction.csv")
score_prediction_output = os.path.join(__current_working__dir,"predictions/score_prediction.csv")
lr_classifier_output = os.path.join(__current_working__dir,'rf_classifier.pkl')
classifier_dot = os.path.join(__current_working__dir,'tree_classifier.dot')
regressor_dot = os.path.join(__current_working__dir,'tree_regressor.dot')
classifier_txt = os.path.join(__current_working__dir,'tree_classifier.txt')
regressor_txt = os.path.join(__current_working__dir,'tree_regressor.txt')
classifier_java = os.path.join(__current_working__dir,'../../Java/src/level_selection/model_representation/DecisionTreeClassifier.java')
forest_classifier_java = os.path.join(__current_working__dir,'../../Java/src/level_selection/model_representation/RandomForestClassifier.java')
regressor_java = os.path.join(__current_working__dir,'../../Java/src/level_selection/model_representation/DecisionTreeRegressor.java')

# Command line: converting .dot into .png
# dot -Tpng input.dot > output.png

# TODO log:
# - Refine the allocation of strategy weights. As of now, weights are not altered during
#   the training phase of a model. Greater discriminability can be achieved if weights are set to
#   dynamically change as part of the model training.


""" Main model processing/ see source directory for model setup.

param: debug    ->  verbose debugging information
"""
def main(debug=False):
    custom_data_split = False
    test_data_from_testing_folder = True
    sort_test_data = True
    drop_losses = True # drops losses and will not calculate score predictions for these instances
    test_size = 0.1 # ratio of training and testing data
    strategies, strategies_weights = dataPrep.get_weighted_strategies()


    # **************** START OF DATA PREP **************** #
    learning_data = dataPrep.read_input(learning_folder, debug)
    testing_data = dataPrep.read_input(testing_folder, debug)
    if sort_test_data: testing_data = testing_data.sort_values('num_score')
    game_outcome_learning = learning_data['game_won'].copy()
    game_outcome_testing = testing_data['game_won'].copy()

    # LEVEL I : binary classifier -> Will the level be won or lost?
    labels_binaryClassifier_learning = learning_data['game_won'].copy()
    for sample in range(0, len(labels_binaryClassifier_learning)):
        if labels_binaryClassifier_learning[sample] == "WON":
            labels_binaryClassifier_learning[sample] = 'won'
        elif labels_binaryClassifier_learning[sample] == "LOST":
            labels_binaryClassifier_learning[sample] = 'lost'
        else:
            raise Exception('*Error: Game outcome is neither WON or LOST.')

    # TODO: allocation of strategy weights (as listed up top)
    numerical_strategies_learning = dataPrep.numerical_transform_strategies(learning_data, strategies, strategies_weights)
    features_binaryClassifier_learning = learning_data.drop(['levelid','num_score','list_strategies','game_won'], axis=1).copy()
    features_binaryClassifier_learning['numerical_strategies'] = numerical_strategies_learning

    numerical_strategies_testing = dataPrep.numerical_transform_strategies(testing_data, strategies, strategies_weights)

    information_value_data = learning_data.drop(['levelid','num_score','list_strategies'], axis=1).copy()
    information_value_data['numerical_strategies'] = numerical_strategies_learning

    information_values = evaluate.calculate_information_values(information_value_data, features_binaryClassifier_learning.columns, features_binaryClassifier_learning.columns, 'game_won', debug=debug)
    print("Information values: ", information_values)


    # LEVEL II : regression -> how many points will the agent score in levels classified to be won?
    labels_regression_learning = learning_data['num_score'].copy()
    features_regression_learning = learning_data.drop(['levelid','num_score','list_strategies','game_won'], axis=1).copy()
    features_regression_learning['numerical_strategies'] = numerical_strategies_learning
    if (drop_losses == True):
        for sample in range(0, len(labels_regression_learning)):
            if str(game_outcome_learning[sample]) == 'LOST':
                labels_regression_learning = labels_regression_learning.drop([sample], axis=0)
                features_regression_learning = features_regression_learning.drop([sample], axis=0)

    if debug:
        print("\n*********\n", "binary classifier:\n", features_binaryClassifier_learning, labels_binaryClassifier_learning.values, "length:", len(labels_binaryClassifier_learning.shape))
        print("\n*********\n", "score regression:\n", features_regression_learning, labels_regression_learning.values, "length:", len(labels_binaryClassifier_learning.shape))


    # LEVEL I : feature and label sets
    ## custom split for training and test data (guarantees predictions for levels 1-21)
    if custom_data_split:
        labels_binaryClassifier_testing = labels_binaryClassifier_learning.iloc[1:22]
        labels_binaryClassifier_training = labels_binaryClassifier_learning.drop(labels_binaryClassifier_learning.index[1:22])
        features_binaryClassifier_testing = features_binaryClassifier_learning.iloc[1:22]
        features_binaryClassifier_training = features_binaryClassifier_learning.drop(features_binaryClassifier_learning.index[1:22])
    ## sklModel split for training and test data
    elif test_data_from_testing_folder:
        # LEVEL I : binary classifier -> Will the level be won or lost?
        labels_binaryClassifier_testing = testing_data['game_won'].copy()
        for sample in range(0, len(labels_binaryClassifier_testing)):
            if labels_binaryClassifier_testing[sample] == "WON":
                labels_binaryClassifier_testing[sample] = 'won'
            elif labels_binaryClassifier_testing[sample] == "LOST":
                labels_binaryClassifier_testing[sample] = 'lost'
            else:
                raise Exception('*Error: Game outcome is neither WON or LOST.')
        
        features_binaryClassifier_testing = testing_data.drop(['levelid','num_score','list_strategies','game_won'], axis=1).copy()
        features_binaryClassifier_testing['numerical_strategies'] = numerical_strategies_testing
        labels_binaryClassifier_training = labels_binaryClassifier_learning
        features_binaryClassifier_training = features_binaryClassifier_learning
        
    else:
        labels_binaryClassifier_training, labels_binaryClassifier_testing,\
        features_binaryClassifier_training, features_binaryClassifier_testing =\
        sklModel.train_test_split(labels_binaryClassifier_learning, features_binaryClassifier_learning, test_size=test_size)


    # LEVEL II : feature and label sets
    ## custom split for training and test data (guarantees predictions for levels 1-21)
    if (custom_data_split == True):
        labels_regression_testing = labels_regression_learning.iloc[1:22]
        labels_regression_training = labels_regression_learning.drop(labels_regression_learning.index[1:22])
        features_regression_testing = features_regression_learning.iloc[1:22]
        features_regression_training = features_regression_learning.drop(features_regression_learning.index[1:22])
    ## sklModel split for training and test data
    elif test_data_from_testing_folder:
        labels_regression_training = labels_regression_learning
        features_regression_training = features_regression_learning
        labels_regression_testing = testing_data['num_score'].copy()
        features_regression_testing = testing_data.drop(['levelid','num_score','list_strategies','game_won'], axis=1).copy()
        features_regression_testing['numerical_strategies'] = numerical_strategies_testing
        if (drop_losses == True):
            for sample in range(0, len(labels_regression_testing)):
                if str(game_outcome_testing[sample]) == 'LOST':
                    labels_regression_testing = labels_regression_testing.drop([sample], axis=0)
                    features_regression_testing = features_regression_testing.drop([sample], axis=0)
    else:
        labels_regression_training, labels_regression_testing,\
        features_regression_training, features_regression_testing =\
        sklModel.train_test_split(labels_regression_learning, features_regression_learning, test_size=test_size)


    # **************** END OF DATA PREP ****************
    print("\n=============Evaluation results for the LEVEL I binary classifier:=============\n")
    # cross compare classifiers from different models
    class_eval = pd.DataFrame()
    class_eval['testing'] = labels_binaryClassifier_testing

    print("\n\n------------- Test of a decision tree classifier -------------")
    # trains the binary classifier on the training data
    classifier = decisionTreeCl.train_classifier(features_binaryClassifier_training, labels_binaryClassifier_training, debug)
    # querys the binary classifier based on the testing features
    pred_classifier, pred_classifier_proba = query.query_classifier(features_binaryClassifier_testing, classifier)
    class_eval['decision_tree'] = pred_classifier

    evaluate.evaluate_prediction_table(labels_binaryClassifier_testing, pred_classifier, binary_prediction_output, debug)
    evaluate.evaluate_prediction_classifier(labels_binaryClassifier_testing, pred_classifier)

    print("\n\n------------- Test of a random forest classifier -------------")
    rf_classifier = randomForestCl.train_rf_classifier(features_binaryClassifier_training, labels_binaryClassifier_training)
    randomForestCl.forest_to_java_code(rf_classifier, list(features_binaryClassifier_training.columns), forest_classifier_java, 'RandomForestClassifier')
    pred_rf_classifier = randomForestCl.query_rf_classifier(rf_classifier, features_binaryClassifier_testing)
    class_eval['random_forest'] = pred_rf_classifier
    evaluate.evaluate_prediction_classifier(labels_binaryClassifier_testing,pred_rf_classifier)
    print("Results are being compared against a single decision tree binary classification below...")

    print("\n\n------------- Test of a logistic regression -------------")
    lr_classifier = LogisticRegression(solver='lbfgs', max_iter=1000).fit(features_binaryClassifier_training, labels_binaryClassifier_training)
    with open(lr_classifier_output, 'wb') as file:
        pickle.dump(lr_classifier, file)
    pred_lr_classifier = lr_classifier.predict(features_binaryClassifier_testing)
    class_eval['logistic_regression'] = pred_lr_classifier
    evaluate.evaluate_prediction_classifier(labels_binaryClassifier_testing,pred_lr_classifier)
    print("Results are being compared against a single decision tree binary classification below...")


    print("\n=============Evaluation results for the LEVEL II regression model:=============\n")
    # cross compare regressor predictions from different models
    pred_eval = pd.DataFrame()
    pred_eval['testing'] = labels_regression_testing

    print("\n\n------------- Test of a decision tree regressor -------------")
    # trains the regression model on the training data
    regressor = decisionTreeRg.train_regressor(features_regression_training, labels_regression_training, debug)
    # querys the regression model based on the testing features
    pred_regressor = query.query_regressor(features_regression_testing, regressor)
    pred_eval['pred_regressor'] = pred_regressor
    evaluate.evaluate_prediction_table(labels_regression_testing, pred_regressor, score_prediction_output, debug)
    evaluate.evaluate_prediction_metrics(labels_regression_testing, pred_regressor, labels_regression_learning)

    print("\n\n------------- Test of a linear model -------------")
    pred_linear_model = linearModel.linear_model(features_regression_training, labels_regression_training, features_regression_testing, labels_regression_testing, debug)
    pred_eval['linear_model'] = pred_linear_model
    evaluate.evaluate_prediction_metrics(labels_regression_testing, pred_linear_model, labels_regression_learning)

    print("\n\n------------- Test of a random forest regressor -------------")
    pred_random_forest = randomForestRg.random_forest(features_regression_training, labels_regression_training, features_regression_testing, labels_regression_testing)
    pred_eval['random_forest'] = pred_random_forest
    evaluate.evaluate_prediction_metrics(labels_regression_testing,pred_random_forest,labels_regression_training)

    # viualize predictions across models
    if (debug):
        print("\nDecision tree vs. random forest vs. linear model regression classifier for score regression:")
        pred_eval_np = pred_eval.to_numpy()
        print(pred_eval_np)
        plt.plot(pred_eval_np)
        plt.show()

        print("\nDecision tree vs. random forest vs. logistic regression classifier for binary lost/won decision making:")
        class_eval_np = class_eval.to_numpy()
        print(class_eval_np)
        evaluate.evaluate_classifiers(class_eval_np)

    # visualize decision trees for both LEVEL I and LEVEL II
    sklTree.export_graphviz(classifier, classifier_dot, feature_names=features_binaryClassifier_learning.columns)
    sklTree.export_graphviz(regressor, regressor_dot, feature_names=features_regression_learning.columns)

    decisionTreeCl.tree_to_java_code(classifier, list(features_binaryClassifier_learning.columns), classifier_java, 'DecisionTreeClassifier')
    decisionTreeCl.tree_to_java_code(regressor, list(features_regression_learning.columns), regressor_java, 'DecisionTreeRegressor')

    with open(classifier_txt, 'w') as file:
        file.write(sklTree.export_text(classifier, feature_names=list(features_binaryClassifier_learning.columns)))
    with open(regressor_txt, 'w') as file:
        file.write(sklTree.export_text(regressor, feature_names=list(features_regression_learning.columns)))


    test_dict = {'num_pigs': [1],'num_birds': [1],'num_destroyable_objects': [1],'num_generated_shots': [1],'num_times_played': [1], 'num_strategies': [1],
                   'num_line_segments_hills': [1],'num_score': [1],'max_score': [1],'levelid': [1],'list_strategies': ['domino'],'game_won': [1],}
    test_dataframe = pd.DataFrame(data=test_dict)
    print("\n------------ Testing external queries ------------")
    print("Resulting test query: ", query.query_predictions(test_dataframe))
    if debug:
        sklTree.plot_tree(classifier)
        sklTree.plot_tree(regressor)
        plt.show()


if __name__ == "__main__":
    main()
