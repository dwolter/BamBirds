#!/usr/bin/env python3

import planner.src.main.python.rebound as rebound
import vision.src.main.python.sciencebirds as vision
import level_selection.src.main.python.prediction as level_selection
import src.main.python.automated_execution as automated_execution
import evaluation

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate testing and learning data and/or evaluate learning by simulation over testing data')

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Show debug output')
    # rebound.add_argument('-v', dest="verbosity", action='count', default='0', help='Increase logging verbosity')


    subparsers = parser.add_subparsers(
        title='commands', description='available subcommands')
    
    automated_execution_parser = subparsers.add_parser('automated_execution', help='Module for automated execution of the agent')
    automated_execution.create_argparser(automated_execution_parser)

    level_selection_parser = subparsers.add_parser(
        'level_selection', help='Level selection module')
    level_selection.create_argparser(level_selection_parser)

    vision_parser = subparsers.add_parser(
        'vision', help='Run the vision module')
    vision.create_argparser(vision_parser)

    evaluation_parser = subparsers.add_parser(
        'evaluation', help='Run the evaluation module')
    evaluation.create_argparser(evaluation_parser)

    rebound_parser = subparsers.add_parser(
        'rebound', help='Run the rebound planner')
    rebound.create_argparser(rebound_parser)

    args = parser.parse_args()
    try:
        args.func(args)
    except Exception as e:
        import traceback
        print(traceback.format_exc())
        parser.print_help()
    parser.exit()
