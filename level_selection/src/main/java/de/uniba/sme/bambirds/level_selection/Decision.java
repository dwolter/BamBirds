package de.uniba.sme.bambirds.level_selection;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import de.uniba.sme.bambirds.common.utils.SelectionAlgorithms;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.objects.Level.State;

/**
 * Class for evaluating probabilities of levels from predictions and level
 * states
 */
public class Decision {
	private static final Logger log = LogManager.getLogger(Decision.class);

	private final boolean primitiveCombination;

	/**
	 * 
	 * @param primitiveCombination if the combination of score prediction and win
	 *                             prediction should be primitive
	 */
	public Decision(boolean primitiveCombination) {
		this.primitiveCombination = primitiveCombination;
	}

	/**
	 * Calculates the probability of selection for each level
	 *
	 * @param maxScores   The until now achieved scores. Structure: {levelid: score}
	 * @param costs       The costs of all levels measured by the time needed to
	 *                    complete
	 * @param predictions The predictions for all levels in the next round.
	 *                    Structure: {levelid: PredictionTuple(scorePrediction,
	 *                    winProbability)}
	 * @param levelStates The states all levels are currently in. Structure:
	 *                    {levelid: State}
	 * @param timeLeft    The remaining time for the competition
	 * @return The probability distribution for all levels. Structure: {levelid:
	 *         probability}
	 */
	public Map<Integer, Double> calculateProbabilityDistribution(Map<Integer, Integer> maxScores,
			Map<Integer, Long> costs, Map<Integer, PredictionTuple<Integer, Double>> predictions,
			Map<Integer, Level.State> levelStates, long timeLeft) {

		double brierScoreLoss = ErrorCalculation.calculateBrierScoreLoss(levelStates, predictions);
		log.debug("Error classifier: " + brierScoreLoss);

		double scoresError = ErrorCalculation.calculateScoreError(maxScores, predictions);
		log.debug("Error scores: " + scoresError);

		Map<Integer, Double> improvementPredictions = calculateImprovementPrediction(maxScores, costs, predictions,
				brierScoreLoss, scoresError, levelStates, timeLeft);
		log.debug("Improvement predictions: " + improvementPredictions.toString());

		return SelectionAlgorithms.softmax(improvementPredictions);
	}



	/**
	 * Calculate the improvement based on the accuracy of the predictions. More info
	 * about the formula can be found in the Project Report 2019 under
	 * Level-Selection->Architecture->Decision
	 * 
	 * @param maxScores      The max achieved scores of the Levels
	 * @param costs          The Costs for a Levels, measured in time needed
	 * @param predictions    The predictions for all Levels
	 * @param brierScoreLoss The error of the Won-Classifier
	 * @param scoresError    The error of the Score-Rregressor
	 * @param levelStates    The states all Levels are in. If state is Resigned,
	 *                       improvement will not be calculated
	 * @param timeLeft       The time left in the competition
	 * @return The improvement prediction, not measured in Score points, only
	 *         difference important
	 */
	private Map<Integer, Double> calculateImprovementPrediction(Map<Integer, Integer> maxScores, Map<Integer, Long> costs,
			Map<Integer, PredictionTuple<Integer, Double>> predictions, double brierScoreLoss, double scoresError,
			Map<Integer, State> levelStates, long timeLeft) {
		Map<Integer, Double> improvementPrediction = new HashMap<>();

		predictions.forEach((key, predictionTuple) -> {
			if (levelStates.get(key) != State.RESIGNED) {
				double value = 0;
				int improvement = predictionTuple.getScore() - maxScores.get(key);
				double improvementWithWon = predictionTuple.getScore() * predictionTuple.getWinProbability()
						- maxScores.get(key);
				if (primitiveCombination) {
					value = improvementWithWon;
				} else {
					value = improvement * brierScoreLoss;
					value += improvementWithWon * scoresError;
					value /= brierScoreLoss + scoresError;
				}

				if (timeLeft > 0) {
					value *= (double) timeLeft / costs.get(key);
				} else {
					value *= (double) 60 / costs.get(key);
				}
				improvementPrediction.put(key, value);
			}
		});

		return improvementPrediction;
	}

}
