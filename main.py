#!/usr/bin/env python3

import os
import subprocess
import time
import numpy as np
import copy

"""
Installation:
see https://pyautogui.readthedocs.io/en/latest/install.html
"""

# Modify to your own satisfaction/ needs
learning_starting_levels = 8
learning_last_levels = 12  # current max: 12
learning_rounds = 5
learning_folder = 'data/learning'

testing_starting_levels = 1
testing_last_levels = 0  # current max: 3
testing_rounds = 5
testing_folder = 'data/testing'

name_feature_file = 'featureList.csv'
levels_folder = 'game/Levels/levels_organised/'
custom_levels_folder = 'game/Levels/custom_levels/levels'
this_folder = os.path.dirname(os.path.abspath(__file__))
bambird_folder = os.path.join(this_folder, '../..')
game_folder = os.path.join(bambird_folder, 'game/slingshot')


def run_server_and_agent(server_args, agent_args):
    with subprocess.Popen(server_args, cwd=bambird_folder, text=True) as server:
        time.sleep(10)
        with subprocess.Popen(agent_args, cwd=bambird_folder) as agent:
            time.sleep(10)
            pyautogui.typewrite('\n')
            while agent.poll() == None:
                time.sleep(30)
            if agent.returncode != 0:
                raise RuntimeError(
                    'Agent did not terminate normally, returncode: '+str(agent.returncode))
        server.terminate()


def generate_data(args):

    import pyautogui
    pyautogui.PAUSE = 1
    
    if args.run_old_server:
        run_server_args = ['ant', 'run-server-old']
    else:
        run_server_args = ['ant', 'run-server', '-Dtime=127', '-Dlevels=21']

    run_client_learning_args = ['ant', 'run', '-Drounds='+str(learning_rounds)]
    run_client_testing_args = ['ant', 'run', '-Drounds='+str(testing_rounds)]
    run_php_args = ['sudo', 'php', '-S', '127.0.0.1:80']

    print("""
    -- Please make sure chrome is running with chrome.angrybirds.com open (Do not mind the connection error for now)
    -- Never hide the reload-button in chrome-window by other windows and wait until the agent is running before highliting anything else than the Server-GUI
    -- Feature-Files in src/Python/prediction/data/{learning|testing} will be overwritten so make sure they are backuped!
    -- make sure the php/python server is running when executing this""")

    remove_features_args = ['rm', name_feature_file]
    remove_features = subprocess.Popen(
        remove_features_args, cwd=bambird_folder)
    if remove_features.wait(20) != 0:
        if remove_features.returncode is 1:
            pass
        else:
            raise RuntimeError(
                'could not remove featureList.csv, returncode: '+str(remove_features.returncode))

    # with subprocess.Popen(run_server_old_args, cwd=bambird_folder, stdin=subprocess.PIPE, text=True) as server:
    #     time.sleep(5)
    for i in range(learning_starting_levels, learning_last_levels+1):

        move_levels_args = ['cp', '-a', os.path.join(bambird_folder, levels_folder, 'learning_'+str(
            i)+'/.'), os.path.join(bambird_folder, custom_levels_folder)]
        move_levels = subprocess.Popen(move_levels_args, cwd=bambird_folder)
        if move_levels.wait(20) != 0:
            raise RuntimeError('could not move learning levels '+str(
                i)+' into custom_levels folder, returncode: '+str(move_levels.returncode))

        reload_location = pyautogui.locateOnScreen(
            os.path.join(this_folder, 'reload.png'), grayscale=True)
        pyautogui.click(
            reload_location[0]+reload_location[2]/2, reload_location[1]+reload_location[3]/2)
        pyautogui.move(0, 100)

        time.sleep(5)
        run_server_and_agent(run_server_args, run_client_learning_args)

        move_features_args = ['mv', name_feature_file, os.path.join(
            learning_folder, 'learning_'+str(i)+'.csv')]
        move_feature_list = subprocess.Popen(
            move_features_args, cwd=bambird_folder)
        if move_feature_list.wait(20) != 0:
            raise RuntimeError(
                'could not move featureList.csv, returncode: '+str(move_feature_list.returncode))

    for i in range(testing_starting_levels, testing_last_levels+1):

        move_levels_args = ['cp', '-a', os.path.join(bambird_folder, levels_folder, 'testing_'+str(
            i)+'/.'), os.path.join(bambird_folder, custom_levels_folder)]
        move_levels = subprocess.Popen(move_levels_args, cwd=bambird_folder)
        if move_levels.wait(20) != 0:
            raise RuntimeError('could not move testing levels'+str(i+1) +
                               ' into custom_levels folder, returncode: '+str(move_levels.returncode))

        reload_location = pyautogui.locateOnScreen(
            os.path.join(this_folder, 'reload.png'), grayscale=True)
        pyautogui.click(
            reload_location[0]+reload_location[2]/2, reload_location[1]+reload_location[3]/2)
        pyautogui.move(0, 100)

        time.sleep(5)
        run_server_and_agent(run_server_args, run_client_testing_args)

        move_features_args = ['mv', name_feature_file, os.path.join(
            testing_folder, 'testing_'+str(i)+'.csv')]
        move_feature_list = subprocess.Popen(
            move_features_args, cwd=bambird_folder)
        if move_feature_list.wait(20) != 0:
            raise RuntimeError(
                'could not move featureList.csv, returncode: '+str(move_feature_list.returncode))
    print('-- Generating finished')


def train_models(args):
    import prediction.learning as learning
    learning.main(debug=args.verbose)


def simulate_level_selection(args):
    import prediction.simulate as simulate
    import matplotlib.pyplot as plt
    from tqdm import tqdm

    data = {}

    testing_dir = os.listdir(os.path.join(bambird_folder, testing_folder))

    for i, file in enumerate(testing_dir):
        if file.endswith('.csv'):
            scores = {}
            filename = os.path.join(bambird_folder, testing_folder, file)

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
                    if len(scores.keys()) is 0:
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
                    if len(scores.keys()) is 0:
                        for key in scores_result.keys():
                            scores[key] = scores_result[key]
                    else:
                        for key in scores_result.keys():
                            scores[key].extend(scores_result[key])
            
            for key in scores.keys():
                for i, score in enumerate(scores[key]):
                    scores[key][i] = score/max_score
            
            if len(data.keys()) is 0:
                data['old'] = [old_score/max_score]
                for key in scores.keys():
                    data[key] = scores[key]
            else:
                data['old'].append(old_score/max_score)
                for key in scores.keys():
                    data[key].extend(scores[key])
            
    plt.boxplot(data.values(),labels=data.keys())
    plt.ylim(0,1)
    plt.show()


def model_evaluation(score_first_round, game_matrix, filename, num_levels, max_attempts, max_scores, verbose):
    import Python.prediction.simulate as simulate
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
    import Python.prediction.simulate as simulate
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


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate testing and learning data and/or evaluate learning by simulation over testing data')

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Show debug output')

    subparsers = parser.add_subparsers(
        title='commands', description='available subcommands')
    generate = subparsers.add_parser(
        'generate', help='generate testing and learning data')
    generate.add_argument('-o', action='store_true',
                          help='Use the old ABServer')
    generate.set_defaults(func=generate_data)

    train = subparsers.add_parser('train', help='Train the models')
    train.set_defaults(func=train_models)

    evaluate = subparsers.add_parser(
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

    args = parser.parse_args()
    args.func(args)

    parser.exit()
