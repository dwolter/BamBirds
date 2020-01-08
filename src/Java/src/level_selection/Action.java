package level_selection;

import java.util.Comparator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import database.LevelStorage;
import helper.CustomLogger;
import meta.Level;

/**
 * Class for selecting a Level using a specific Strategy and the probability
 * distribution calculated by {@link Decision}
 */
public class Action {

	public enum Strategy {
		OLD, GREEDY, EPSILON_GREEDY, EPSILON_ADAPTIVE, RANDOM
	}

	private double epsilonInit;
	private Strategy strategy;
	private Random random;
	private long timeLimit;
	private int startLevel;
	private int numberOfLevels;

	// Old Level Selection Variables
	private int currentLevel;
	private int numberOfTimesPlayedTheMost = -1;
	private int numberOfTimesPlayedMinimum = 1000;
	private boolean allLostLevelsPlayedTwice = false;
	private int lastLostLevel = -1;

	/**
	 * Create a new Action Object
	 *
	 * @param epsilon        initial value for epsilon algorithms
	 * @param strategy       Type of strategy used to select next level
	 * @param timeLimit      Maximum time in the Competition
	 * @param startLevel     The first level the agent is playing
	 * @param numberOfLevels The number of Levels the agent is playing
	 */
	public Action(double epsilon, Strategy strategy, long timeLimit, int startLevel, int numberOfLevels) {
		this(epsilon, strategy, timeLimit, startLevel, numberOfLevels, System.currentTimeMillis());
	}

	/**
	 * Create a new Action Object
	 *
	 * @param epsilon        initial value for epsilon algorithms
	 * @param strategy       Type of strategy used to select next level
	 * @param timeLimit      Maximum time in the Competition
	 * @param startLevel     The first level the agent is playing
	 * @param numberOfLevels The number of Levels the agent is playing
	 * @param seed           The seed for the random number generator
	 */
	public Action(double epsilon, Strategy strategy, long timeLimit, int startLevel, int numberOfLevels, long seed) {
		this.random = new Random(seed);
		this.epsilonInit = epsilon;
		this.strategy = strategy;
		this.timeLimit = timeLimit;
		this.numberOfLevels = numberOfLevels;
		this.startLevel = startLevel;
	}

	/**
	 * Calculate the next Level to be played depending on the strategy.
	 *
	 * @param probabilities for each possible level
	 * @param timeLeft      remaining time
	 * @return the next level to play
	 */
	public int nextLevel(Map<Integer, Double> probabilities, long timeLeft) {
		switch (strategy) {
		case OLD:
			return oldSelection();
		case GREEDY:
			return greedySelection(probabilities);
		case EPSILON_GREEDY:
			return epsilonGreedySelection(probabilities);
		case EPSILON_ADAPTIVE:
			return epsilonAdaptiveSelection(probabilities, timeLeft);
		case RANDOM:
			return randomSelection(probabilities);
		default:
			// Use complete randomness if no strategy is selected
			return random.nextInt(numberOfLevels) + startLevel;
		}
	}

	/**
	 * The old Level Selection, where first all not won levels will be attempted 3
	 * times. Then if less than 15 percent have been lost, the Level with the
	 * highest possible improvement based on the maximum possible points will be
	 * selected
	 *
	 * @return the next level to play
	 */
	private int oldSelection() {

		LevelStorage levelStorage = LevelStorage.getInstance();
		Set<Integer> playedLevels = levelStorage.getListOfIDs();

		double[] probs = new double[playedLevels.size()];
		int bestLevelIndex = -1;
		double bestProp = -10000;
		boolean containsLost = false;
		int lostCounter = 0;

		Level lostLevel = levelStorage.getLevelById(lastLostLevel);
		if (lastLostLevel > 0 && lostLevel != null && lostLevel.getBestScore() != 0) {
			allLostLevelsPlayedTwice = true;
		} else {
			lastLostLevel = -1;
		}

		int i = 0;
		for (Integer lvl : playedLevels) {
			Level level = levelStorage.getLevelById(lvl);

			if (numberOfTimesPlayedTheMost < level.numberOfTimesPlayed) {
				numberOfTimesPlayedTheMost = level.numberOfTimesPlayed;
			}

			if (numberOfTimesPlayedMinimum > level.numberOfTimesPlayed) {
				numberOfTimesPlayedMinimum = level.numberOfTimesPlayed;
			}

			double eMaxPoints = level.getEstimatedMaximalPoints();
			double bestScore = level.getBestScore();

			double probability = 1 - (bestScore / eMaxPoints);

			if (bestScore > eMaxPoints) {
				probs[i] = 0;
			} else {
				probs[i] = probability;
			}

			if (probs[i] == 1) {
				containsLost = true;
				lostCounter++;
				lastLostLevel = level.levelId;
			}
			++i;
			CustomLogger.info(
					"Prob for Level " + level.levelId + " " + probability + ", Played: " + level.numberOfTimesPlayed + " times");
		}

		int j = 0;
		for (Integer lvl : playedLevels) {
			Level level = levelStorage.getLevelById(lvl);

			// First redo all lost levels at most 2 times
			if (!allLostLevelsPlayedTwice && containsLost && probs[j] == 1 && numberOfTimesPlayedTheMost <= 3
					&& level.numberOfTimesPlayed < 3) {
				bestLevelIndex = level.levelId;
				bestProp = probs[j];

				if (level.levelId == lastLostLevel && level.numberOfTimesPlayed == 2) {
					allLostLevelsPlayedTwice = true;
				}
				break;
			}

			// if all lost levels were played twice and there are still some lost levels
			if (allLostLevelsPlayedTwice && containsLost) {
				// if more than 15% of the total number of levels are lost levels, try it again
				// only one more time
				if (lostCounter > (0.15 * numberOfLevels) && probs[j] == 1 && level.numberOfTimesPlayed == 3) {
					bestLevelIndex = level.levelId;
					bestProp = probs[j];
					CustomLogger
							.info("[Meta] All lost levels have been played at most twice but there are still lost levels remaining.");
					CustomLogger.info("[Meta] Too many lost levels left so try one last lost level again.");
					break;
					// else give up on these levels
				} else if ((lostCounter < (0.15 * numberOfLevels)) | (numberOfTimesPlayedTheMost >= 4)) {
					containsLost = false;
					CustomLogger
							.info("[Meta] All lost levels have been played at most twice but there are still lost levels remaining.");
					CustomLogger.info("[Meta] Give up on the lost levels.");
				}
			}

			// If there are no lost levels anymore
			// ignore the lost levels
			// Choose the level with the highest probability
			if (!containsLost && probs[j] != 1 && probs[j] > bestProp
					&& level.numberOfTimesPlayed < (numberOfTimesPlayedMinimum + 2)) {
				bestLevelIndex = level.levelId;
				bestProp = probs[j];
			}
			++j;
		}

		if (bestProp == -10000) {
			CustomLogger.warning("Something went wrong ... Choosing random level...");
			bestLevelIndex = new Random().nextInt(numberOfLevels) + startLevel;
		}

		this.currentLevel = bestLevelIndex;

		CustomLogger.info("Selecting Level " + currentLevel + " Probability: " + (bestProp * 100) + "%");

		return currentLevel;
	}

	/**
	 * Select the level with the highest probability
	 *
	 * @param probabilities for each possible level
	 * @return the next level to play
	 */
	private int greedySelection(Map<Integer, Double> probabilities) {
		int selectedLevel = probabilities.entrySet().stream().max(Comparator.comparingDouble(entry -> entry.getValue()))
				.get().getKey();
		CustomLogger.info(
				"Greedy selected level " + selectedLevel + " which had a probability of " + probabilities.get(selectedLevel));
		return selectedLevel;
	}

	/**
	 * Select the level with the highest probability with probability 1 - epsilon.
	 *
	 * Otherwise randomSelection
	 *
	 * @param probabilities for each possible level
	 * @return the next level to play
	 */
	private int epsilonGreedySelection(Map<Integer, Double> probabilities) {
		if (random.nextDouble() < epsilonInit) {
			return randomSelection(probabilities);
		} else {
			return greedySelection(probabilities);
		}
	}

	/**
	 * Select the level with the highest probability depending on how much time is
	 * left and how good the score and win predictions are.<br/>
	 * Otherwise randomSelection
	 *
	 * @param probabilities for each possible level
	 * @param timeLeft      remaining time
	 * @return the next level to play
	 */
	private int epsilonAdaptiveSelection(Map<Integer, Double> probabilities, long timeLeft) {
		double scoreError = ErrorCalculation.getScoreError();
		double wonError = ErrorCalculation.getWonError();
		if (scoreError < 0 || wonError < 0) {
			return randomSelection(probabilities);
		}
		double meanError = ((scoreError + wonError) / 2);
		double timeconstraint = 0;
		if (timeLimit > 0) {
			timeconstraint = timeLeft / timeLimit;
		}
		double epsilon = Math.min(meanError + timeconstraint, 1);
		CustomLogger.info("Epsilon: " + epsilon);
		if (random.nextDouble() < epsilon) {
			return randomSelection(probabilities);
		} else {
			return greedySelection(probabilities);
		}
	}

	/**
	 * Randomly select a level depending on its probability
	 *
	 * @param probabilities for each possible level
	 * @return the next level to play
	 */
	private int randomSelection(Map<Integer, Double> probabilities) {
		double r = random.nextDouble();
		double countWeight = 0.0;
		for (Entry<Integer, Double> level : probabilities.entrySet()) {
			countWeight += level.getValue();
			if (countWeight >= r) {
				CustomLogger
						.info("Randomly selected level " + level.getKey() + " which had a probability of " + level.getValue());
				return level.getKey();
			}
		}
		int selectedLevel = random.nextInt(numberOfLevels) + startLevel;
		CustomLogger.info("Randomly selected Level " + selectedLevel);
		return selectedLevel;
	}

}
