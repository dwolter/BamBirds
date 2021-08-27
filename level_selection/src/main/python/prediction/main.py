import argparse
import copy
import os
from . import simulate
from . import learning

__dir_path__ = os.path.dirname(os.path.abspath(__file__))
bambird_folder = os.path.abspath(os.path.join(__dir_path__, '..','..','..','..', '..'))
testing_data_folder = os.path.abspath(os.path.join(bambird_folder, 'data','testing'))

def train_models(args):
    learning.main(debug=args.verbose)


def simulate_level_selection(args):
    import matplotlib.pyplot as plt
    from tqdm import tqdm

    data = {}
    print(testing_data_folder)

    testing_dir = os.listdir(testing_data_folder)

    for i, file in enumerate(testing_dir):
        if file.endswith('.csv'):
            scores = {}
            filename = os.path.join(bambird_folder, testing_data_folder, file)

            print(file + ':')

            max_scores, game_matrix = simulate.initialize_game_matrix(
                filename, args.levels)

            score_first_round = sum(max_scores.values())

            max_score = simulate.calculate_max_score(
                args.attempts, copy.deepcopy(game_matrix), copy.deepcopy(max_scores))

            old_score = simulate.old_level_selection(copy.deepcopy(
                game_matrix), filename, args.levels, args.attempts, copy.deepcopy(max_scores))

            if args.models:
                for j in tqdm(range(args.iterations)):
                    # TODO: This should be multithreaded for better performance
                    scores_result = model_evaluation(score_first_round, copy.deepcopy(
                        game_matrix), filename,
                        args.levels, args.attempts, copy.deepcopy(max_scores), args.verbose)
                    if len(scores.keys()) == 0:
                        for key in scores_result.keys():
                            scores[key] = scores_result[key]
                    else:
                        for key in scores_result.keys():
                            scores[key].extend(scores_result[key])
            else:
                for j in tqdm(range(args.iterations)):
                    # TODO: This should be multithreaded for better performance
                    scores_result = alg_evaluation(score_first_round, copy.deepcopy(
                        game_matrix), filename,
                        args.levels, args.attempts, copy.deepcopy(max_scores), args.verbose, args.classifier, args.regressor)
                    if len(scores.keys()) == 0:
                        for key in scores_result.keys():
                            scores[key] = scores_result[key]
                    else:
                        for key in scores_result.keys():
                            scores[key].extend(scores_result[key])

            for key in scores.keys():
                for i, score in enumerate(scores[key]):
                    scores[key][i] = score/max_score

            if len(data.keys()) == 0:
                data['old'] = [old_score/max_score]
                for key in scores.keys():
                    data[key] = scores[key]
            else:
                data['old'].append(old_score/max_score)
                for key in scores.keys():
                    data[key].extend(scores[key])

    plt.boxplot(data.values(), labels=data.keys())
    plt.ylim(0, 1)
    plt.show()


def model_evaluation(score_first_round, game_matrix, filename, num_levels, max_attempts, max_scores, verbose):
    
    scores = {}
    # TODO: This should be multithreaded for better performance
    for classifier in ['tree', 'forest', 'logistic']:
        for regressor in ['tree', 'forest', 'linear']:
            if classifier+'_'+regressor not in scores:
                scores[classifier+'_'+regressor] = []
            scores[classifier+'_'+regressor].append(simulate_level_selection(copy.deepcopy(game_matrix), filename, num_levels, max_attempts, copy.deepcopy(
                max_scores), algorithm=simulate.Algorithm.GREEDY, enable_softmax=False, regressor_type=regressor, classifier_type=classifier, verbose=verbose))

            scores[classifier+'_'+regressor].append(simulate_level_selection(copy.deepcopy(game_matrix), filename, num_levels, max_attempts, copy.deepcopy(
                max_scores), algorithm=simulate.Algorithm.EPSILON_GREEDY, enable_softmax=True, regressor_type=regressor, classifier_type=classifier, verbose=verbose))

            scores[classifier+'_'+regressor].append(simulate.simulate_level_selection(copy.deepcopy(game_matrix), filename, num_levels, max_attempts, copy.deepcopy(
                max_scores), algorithm=simulate.Algorithm.ADAPTIVE_EPSILON, enable_softmax=True, regressor_type=regressor, classifier_type=classifier, verbose=verbose))
    return scores


def alg_evaluation(score_first_round, game_matrix, filename, num_levels, max_attempts, max_scores, verbose, classifier, regressor):
    scores = {simulate.Algorithm.GREEDY.value: [], simulate.Algorithm.EPSILON_GREEDY.value: [
    ], simulate.Algorithm.ADAPTIVE_EPSILON.value: []}
    # TODO: This should be multithreaded for better performance

    scores[simulate.Algorithm.GREEDY.value].append(simulate.simulate_level_selection(copy.deepcopy(game_matrix), filename, num_levels, max_attempts, copy.deepcopy(
        max_scores), algorithm=simulate.Algorithm.GREEDY, enable_softmax=False, regressor_type=regressor, classifier_type=classifier, verbose=verbose))

    scores[simulate.Algorithm.EPSILON_GREEDY.value].append(simulate.simulate_level_selection(copy.deepcopy(game_matrix), filename, num_levels, max_attempts, copy.deepcopy(
        max_scores), algorithm=simulate.Algorithm.EPSILON_GREEDY, enable_softmax=True, regressor_type=regressor, classifier_type=classifier, verbose=verbose))

    scores[simulate.Algorithm.ADAPTIVE_EPSILON.value].append(simulate.simulate_level_selection(copy.deepcopy(game_matrix), filename, num_levels, max_attempts, copy.deepcopy(
        max_scores), algorithm=simulate.Algorithm.ADAPTIVE_EPSILON, enable_softmax=True, regressor_type=regressor, classifier_type=classifier, verbose=verbose))
    return scores

def create_argparser(parser: argparse.ArgumentParser):
    subparser = parser.add_subparsers(title='Level Selection', description='available commands')
    train = subparser.add_parser('train', help='Train the models')
    train.set_defaults(func=train_models)

    evaluate = subparser.add_parser(
        'evaluate', help='Evaluate models by simulation over testing data')
    evaluate.add_argument('-i', '--iterations', type=int, default=10,
                          help='Number of iterations for the simulation')
    evaluate.add_argument('--levels', type=int, default=21,
                          help='Number of levels to play')
    evaluate.add_argument('--attempts', type=int, default=8,
                          help='Maximal number of attempts after the first round')
    evaluate.add_argument('-e', '--epsilon', type=float, default=0.1,
                          help='Epsilon value for the Epsilon Algorithms')
    evaluate.add_argument('--models', action='store_true',
                          help='Evaluate model performance (default is evaluation of algorithms)')
    evaluate.add_argument('-c', '--classifier', choices=[
                          'tree', 'logistic', 'forest'], default='tree', help='Classifier used if evaluating algorithms')
    evaluate.add_argument('-r', '--regressor', choices=[
                          'tree', 'linear', 'forest'], default='tree', help='Regressor used if evaluating algorithms')

    evaluate.set_defaults(func=simulate_level_selection)
