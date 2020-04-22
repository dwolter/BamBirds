package de.uniba.sme.bambirds.level_selection;

import java.util.Map;
import java.util.Map.Entry;

import de.uniba.sme.bambirds.common.objects.Level.State;

public class ErrorCalculation {

	private static double scoreError = 1;
	private static double brierScoreLoss = 1;

	/**
	 * MeanSquaredError = sum{1 - N}((predicted - actual)^2) / N 
	 * normalized : sqrt(MSE)/ meanScore
	 * 
	 * @param maxScores
	 * @param predictions
	 * @return normalized MSE
	 */
	public static double calculateScoreError(Map<Integer, Integer> maxScores,
			Map<Integer, PredictionTuple<Integer, Double>> predictions) {
		double errorSum = 0.0;
		double scoreSum = 0.0;
		for (Entry<Integer, PredictionTuple<Integer, Double>> prediction : predictions.entrySet()) {
			// calculate error and scoreSum ONLY for won levels
			if (maxScores.get(prediction.getKey()) <= 0) {
				continue;
			}

			errorSum += Math.pow((maxScores.get(prediction.getKey()) - prediction.getValue().getScore()), 2);
			scoreSum += maxScores.get(prediction.getKey());
		}

		if(scoreSum == 0){
			// TODO: All Levels seem to be lost until now. How big is the error then?
			return 1;
		}
		// (errorSum / N) / (scoreSum / N)
		scoreError = Math.sqrt(errorSum) / scoreSum;
		return scoreError;
	}

	/**
	 * BrierScoreLoss = sum{1 - N}((probability - actual)^2) / N
	 * 
	 * @param levelStates current States of the Levels
	 * @param predictions predictions for the levels
	 * @return BrierScoreLoss(States, WinPredictions)
	 */
	public static double calculateBrierScoreLoss(Map<Integer, State> levelStates,
			Map<Integer, PredictionTuple<Integer, Double>> predictions) {
		// BrierScoreLoss ~ MSE = sum{1 -> N}((probability - actual)^2) /N
		double sum = 0.0;
		for (Entry<Integer, PredictionTuple<Integer, Double>> prediction : predictions.entrySet()) {
			sum += Math.pow((prediction.getValue().getWinProbability()
					- (levelStates.getOrDefault(prediction.getKey(), State.LOST) == State.WON ? 1 : 0)), 2);
		}
		brierScoreLoss = sum / predictions.size();
		return brierScoreLoss;
	}

	/**
	 * @return The last calculated scoreError
	 */
	public static double getScoreError() {
		return scoreError;
	}

	/**
	 * @return The last calculated wonError
	 */
	public static double getWonError() {
		return brierScoreLoss;
	}

}
