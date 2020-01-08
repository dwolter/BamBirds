package level_selection;

import static helper.Constants.PERFORMANCE_MEASUREMENT_ENABLED;
import static helper.Constants.LEVEL_SELECTION_FIRST_ROUND_ITERATIVE;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.Set;

import database.LevelStorage;
import helper.CustomLogger;
import level_selection.Action.Strategy;
import level_selection.Prediction.ClassifierType;
import level_selection.Prediction.RegressorType;
import meta.Level;
import meta.Level.State;

public class LevelSelection {
	private int numberOfLevels;
	private int currentLevel;
	private int startLevel;
	private int roundsMax;
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
	 * @param startlevel     The first Level to be played
	 * @param rounds         The number of rounds the agent will play (Only
	 *                       considered if
	 *                       helper.Constants.PERFORMANCE_MEASUREMENT_ENABLED)
	 * @param timeLimit      The time limit for the competition
	 * @param range          The range in which Levels will be played (essentially
	 *                       numberofLevels from startlevel)
	 */
	public LevelSelection(int numberOfLevels, int startlevel, int rounds, int timeLimit, int range) {
		if (range > 0) {
			this.numberOfLevels = range;
		} else {
			this.numberOfLevels = numberOfLevels;
		}
		this.startLevel = startlevel;
		this.roundsMax = rounds;

		this.remainingTime = timeLimit * 60;

		this.roundsPlayed = 0;
		this.currentLevel = 0;

		this.currentIteration = 0;

		this.allLevelsPlayedOnce = false;

		// Currently best performing pair of Cassifier and Regressor is Decision Tree
		// and Linear Model
		ClassifierType classifierType = ClassifierType.DECISION_TREE;
		RegressorType regressorType = RegressorType.LINEAR_MODEL;
		if (range > 0) {
			this.prediction = new Prediction(startlevel, Math.min(range, numberOfLevels), classifierType, regressorType);
		} else {
			this.prediction = new Prediction(startlevel, numberOfLevels, classifierType, regressorType);
		}
		this.decision = new Decision(true);
		this.action = new Action(0.2, Strategy.EPSILON_ADAPTIVE, timeLimit, startlevel, numberOfLevels);
	}

	public int selectNextLevel() {
		// Only called once from Bambird.start()
		if (currentLevel == 0) {
			this.currentLevel = this.startLevel;

			if (PERFORMANCE_MEASUREMENT_ENABLED) {
				CustomLogger.info("Remaining rounds to play: " + (roundsMax - roundsPlayed));
				this.roundsPlayed++; // Increment at start to play the correct number of times
			}

			// since numOfLevels is zero-index, but our levelIds start at 1
			if (currentLevel == 0)
				currentLevel = 1;
		} else if (LEVEL_SELECTION_FIRST_ROUND_ITERATIVE || PERFORMANCE_MEASUREMENT_ENABLED) { // select levels in first
																																														// round iteratively
			this.currentLevel++;

			// stay in range of the numberOfLevels
			if (this.currentLevel > this.numberOfLevels + this.startLevel - 1) {
				// If we are playing in sequence check if we have a maximum of rounds to play
				if (PERFORMANCE_MEASUREMENT_ENABLED) {
					if (this.roundsPlayed < this.roundsMax) {
						CustomLogger.info("Remaining rounds to play: " + (this.roundsMax - this.roundsPlayed));
						this.roundsPlayed++; // So long a round to play is left decrement the number when end of levels is
																	// reached
					} else if (this.roundsMax <= -1) { // -1 means no boundaries so nothing to do here
					} else {
						CustomLogger.info("No rounds remaining, requesting shutdown...");
						this.currentLevel = -1;
						return -1; // when no rounds are left to play return -1 to signal that program shall
												// terminate
					}
				}
				this.currentLevel = startLevel;

				// we ran out of bounds of all levels, so all levels must have been played at
				// least ONCE
				if (!this.allLevelsPlayedOnce) {
					this.allLevelsPlayedOnce = true;
				}
			}
			if (PERFORMANCE_MEASUREMENT_ENABLED) {
				CustomLogger.info("Selecting next level in order because PERFORMANCE_MEASUREMENT is enabled");
				return currentLevel;
			}
		}

		LevelStorage levelStorage = LevelStorage.getInstance();
		Set<Integer> playedLevels = levelStorage.getListOfIDs();

		if (!this.allLevelsPlayedOnce) {
			int storageSize = levelStorage.getStorageSize();

			// if that much levels have been stored, all levels must have been played ONCE
			if (storageSize >= this.numberOfLevels) {
				this.allLevelsPlayedOnce = true;
				CustomLogger.info("LevelStorage has size " + storageSize + ", so all levels must have been played once!");
			} else {
				CustomLogger.info("LevelStorage has size " + storageSize);
			}
		}

		if (this.allLevelsPlayedOnce) {
			CustomLogger.info("DECISION TREE MANAGER: NOW starts prediction");
			Map<Integer, PredictionTuple<Integer, Double>> predictions = prediction.predict();

			CustomLogger.info(predictions.toString());

			Map<Integer, Integer> maxScores = new HashMap<>();
			Map<Integer, Long> costs = new HashMap<>();
			Map<Integer, State> levelStates = new HashMap<>();

			for (Integer levelId : playedLevels) {
				Level level = levelStorage.getLevelById(levelId);
				maxScores.put(levelId, level.getBestScore());
				costs.put(levelId, level.getCosts());
				levelStates.put(levelId, level.getLevelState());
			}

			CustomLogger.info("Achieved scores: " + maxScores);

			Map<Integer, Double> probabilities = decision.calculateProbabilityDistribution(maxScores, costs, predictions,
					levelStates, this.remainingTime);

			CustomLogger.info("Probabilities: " + probabilities);

			currentLevel = action.nextLevel(probabilities, this.remainingTime);
		} else { // select UNPLAYED levels randomly if all levels have not been played ONCE
			CustomLogger.info("Selecting a random, unplayed level...");

			Random generator = new Random();
			int randomLevel = startLevel;

			// get a random level until we get an unplayed one
			do {
				randomLevel = generator.nextInt(this.numberOfLevels) + this.startLevel;
			} while (levelStorage.getLevelById(randomLevel) != null);

			CustomLogger.info("Selected randomly level " + randomLevel);

			this.currentLevel = randomLevel;
		}
		return currentLevel;
	}

	// TODO Level über Konsole auswählen
	public int loadLevelByConsoleInput() {

		try (Scanner consoleInput = new Scanner(System.in)) {

			CustomLogger.info("Please enter the required Level!");
			int levelToBeLoaded = consoleInput.nextInt();

			if (numberOfLevels >= levelToBeLoaded) {
				return levelToBeLoaded;
			} else {
				CustomLogger.info("This level is not known or your input isn't a number. Therefore Level " + levelToBeLoaded
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
}
