package de.uniba.sme.bambirds.level_selection;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.database.LevelStorage;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.level_selection.Action.Strategy;
import de.uniba.sme.bambirds.level_selection.Prediction.ClassifierType;
import de.uniba.sme.bambirds.level_selection.Prediction.RegressorType;
import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.objects.Level.State;

public class LevelSelection {
	private static final Logger log = LogManager.getLogger(LevelSelection.class);
	private int numberOfLevels;
	private int currentLevel;
	private int roundsPlayed;

	private boolean allLevelsPlayedOnce;

	/**
	 * Describes how many times the agent currently has started all reached levels
	 * in total
	 */
	private int currentIteration;

	private Prediction prediction;
	private Decision decision;
	private Action action;

	/** The remaining time in seconds. Updated after each level playthrough. */
	private long remainingTime;

	/**
	 * Create a new LevelSelection
	 * 
	 * @param numberOfLevels The number of Levels in total
	 * @param timeLimit      The time limit for the competition in minutes
	 */
	public LevelSelection(int numberOfLevels, int timeLimit) {
		if (Settings.LEVEL_RANGE > 0) {
			this.numberOfLevels = Settings.LEVEL_RANGE;
		} else {
			this.numberOfLevels = numberOfLevels;
		}

		this.remainingTime = timeLimit * 60;

		this.roundsPlayed = 0;
		this.currentLevel = 0;

		this.currentIteration = 0;

		this.allLevelsPlayedOnce = false;

		// Currently best performing pair of Cassifier and Regressor is Decision Tree
		// and Linear Model
		ClassifierType classifierType = ClassifierType.DECISION_TREE;
		RegressorType regressorType = RegressorType.LINEAR_MODEL;
		this.prediction = new Prediction(numberOfLevels, classifierType, regressorType);
		this.decision = new Decision(true);
		this.action = new Action(0.2, Strategy.EPSILON_ADAPTIVE, this.remainingTime, numberOfLevels);
	}

	public int selectNextLevel() {
		// Only called once from Bambird.start()
		if (currentLevel == 0) {
			this.currentLevel = Settings.START_LEVEL;

			if (Settings.DISABLE_LEVEL_SELECTION) {
				log.debug("Remaining rounds to play: " + (Settings.ROUNDS - roundsPlayed));
				this.roundsPlayed++; // Increment at start to play the correct number of times
			}

			// since numOfLevels is zero-index, but our levelIds start at 1
			if (currentLevel == 0)
				currentLevel = 1;
		} else if (Settings.LEVEL_SELECTION_FIRST_ROUND_ITERATIVE || Settings.DISABLE_LEVEL_SELECTION) { // select levels in first
																																														// round iteratively
			this.currentLevel++;

			// stay in range of the numberOfLevels
			if (this.currentLevel > this.numberOfLevels + Settings.START_LEVEL - 1) {
				// If we are playing in sequence check if we have a maximum of rounds to play
				if (Settings.DISABLE_LEVEL_SELECTION) {
					if (this.roundsPlayed < Settings.ROUNDS) {
						log.debug("Remaining rounds to play: " + (Settings.ROUNDS - this.roundsPlayed));
						this.roundsPlayed++; // So long a round to play is left decrement the number when end of levels is
																	// reached
					} else if (Settings.ROUNDS <= -1) { // -1 means no boundaries so nothing to do here
					} else {
						log.debug("No rounds remaining, requesting shutdown...");
						this.currentLevel = -1;
						return -1; // when no rounds are left to play return -1 to signal that program shall
												// terminate
					}
				}
				this.currentLevel = Settings.START_LEVEL;

				// we ran out of bounds of all levels, so all levels must have been played at
				// least ONCE
				if (!this.allLevelsPlayedOnce) {
					this.allLevelsPlayedOnce = true;
				}
			}
		}
		
		if (Settings.DISABLE_LEVEL_SELECTION) {
			log.info("Selecting next level in order because Settings.DISABLE_LEVEL_SELECTION is true");
			return currentLevel;
		}

		LevelStorage levelStorage = LevelStorage.getInstance();
		Set<Integer> playedLevels = levelStorage.getListOfIDs();

		if (!this.allLevelsPlayedOnce) {
			int storageSize = levelStorage.getStorageSize();

			// if that much levels have been stored, all levels must have been played ONCE
			if (storageSize >= this.numberOfLevels) {
				this.allLevelsPlayedOnce = true;
				log.debug("LevelStorage has size " + storageSize + ", so all levels must have been played once!");
			} else {
				log.debug("LevelStorage has size " + storageSize);
			}
		}

		if (this.allLevelsPlayedOnce) {
			Map<Integer, PredictionTuple<Integer, Double>> predictions = prediction.predict();

			log.debug("Predictions: {}", predictions.toString());

			Map<Integer, Integer> maxScores = new HashMap<>();
			Map<Integer, Long> costs = new HashMap<>();
			Map<Integer, State> levelStates = new HashMap<>();

			for (Integer levelId : playedLevels) {
				Level level = levelStorage.getLevelById(levelId);
				maxScores.put(levelId, level.getBestScore());
				costs.put(levelId, level.getCosts());
				levelStates.put(levelId, level.getLevelState());
			}

			log.debug("Achieved scores: {}", maxScores);

			Map<Integer, Double> probabilities = decision.calculateProbabilityDistribution(maxScores, costs, predictions,
					levelStates, this.remainingTime);

			log.debug("Probabilities: {}", probabilities);

			currentLevel = action.nextLevel(probabilities, this.remainingTime);
		} else if(!Settings.LEVEL_SELECTION_FIRST_ROUND_ITERATIVE) { // select UNPLAYED levels randomly if all levels have not been played ONCE
			log.debug("Selecting a random, unplayed level...");

			Random generator = new Random();
			int randomLevel = Settings.START_LEVEL;

			// get a random level until we get an unplayed one
			do {
				randomLevel = generator.nextInt(this.numberOfLevels) + Settings.START_LEVEL;
			} while (levelStorage.getLevelById(randomLevel) != null);

			log.info("Selected randomly level " + randomLevel);

			this.currentLevel = randomLevel;
		}
		return currentLevel;
	}

	// TODO Level über Konsole auswählen
	public int loadLevelByConsoleInput() {

		try (Scanner consoleInput = new Scanner(System.in)) {

			log.info("Please enter the required Level!");
			int levelToBeLoaded = consoleInput.nextInt();

			if (numberOfLevels >= levelToBeLoaded) {
				return levelToBeLoaded;
			} else {
				log.info("This level is not known or your input isn't a number. Therefore Level " + levelToBeLoaded
						+ " cannot be loaded! Please try again!");
				return selectNextLevel();
			}
		}
	}

	public void increaseLevelIteration() {
		this.currentIteration++;
	}

	public void decreaseRemainingTime(long delta) {
		this.remainingTime -= delta;
	}

	public long getRemainingTime() {
		return this.remainingTime;
	}

	public int getCurrentIteration() {
		return this.currentIteration;
	}

	public Prediction getPrediction() {
		return prediction;
	}

	public Decision getDecision() {
		return decision;
	}

	public Action getAction() {
		return action;
	}
}
