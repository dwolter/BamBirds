#!/usr/bin/env python3

import os
import subprocess
import time
import numpy as np
import copy
import argparse
import platform

# Modify to your own satisfaction/ needs
learning_starting_levels = 8
learning_last_levels = 12  # current max: 12
learning_rounds = 5
learning_folder = os.path.join('data', 'learning')

testing_starting_levels = 1
testing_last_levels = 0  # current max: 3
testing_rounds = 5
testing_folder = os.path.join('data', 'testing')

name_feature_file = 'featureList.csv'
levels_folder = os.path.join('game', 'Levels', 'levels_organised')
custom_levels_folder = os.path.join('game', 'Levels', 'custom_levels', 'levels')
this_folder = os.path.dirname(os.path.abspath(__file__))
bambird_folder = os.path.join(this_folder, '..', '..', '..')
game_folder = os.path.join(bambird_folder, 'game', 'slingshot')

operatingSystem = platform.system()
if operatingSystem == "Windows":
    gradle_wrapper = 'gradlew.bat'
else:
    gradle_wrapper = './gradlew'

def run_server_and_agent(server_args, agent_args):
    with subprocess.Popen(server_args, cwd=bambird_folder, text=True) as server:
        time.sleep(10)
        with subprocess.Popen(agent_args, cwd=bambird_folder) as agent:

            while agent.poll() == None:
                time.sleep(30)
            if agent.returncode != 0:
                raise RuntimeError(
                    'Agent did not terminate normally, returncode: '+str(agent.returncode))
        server.terminate()


def generate_data(args):
    
    run_server_args = [gradle_wrapper, 'game:abserver:run', '--args=-t -1 -l 21 --nogui true --autostart true', '--console', 'plain']

    run_client_learning_args = [gradle_wrapper, ':run', f'--args=--rounds {str(learning_rounds)} --disable-level-selection --start-level 1 --range 0 --export-level-stats', '--console', 'plain']
    run_client_testing_args = [gradle_wrapper, ':run', f'--args=--rounds {str(testing_rounds)} --disable-level-selection --start-level 1 --range 0 --export-level-stats', '--console', 'plain']

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

def create_argparser(parser: argparse.ArgumentParser):
    subparsers = parser.add_subparsers(title='commands', description="Available subcommands")
    generate = subparsers.add_parser(
        'generate_indepth_data', help='generate level information for testing and learning levels')
    
    generate.set_defaults(func=generate_data)
