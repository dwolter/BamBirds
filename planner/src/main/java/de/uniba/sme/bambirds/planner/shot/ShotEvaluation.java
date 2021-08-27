package de.uniba.sme.bambirds.planner.shot;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.Triplet;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;

public class ShotEvaluation {
	private static final Logger log = LogManager.getLogger();

  public enum EvaluationResult {
    GOOD,
    BAD
  }
  
	public static EvaluationResult evaluate(Plan chosenPlan, Level level, int shotIndex) {
		EvaluationResult resultEvaluation = EvaluationResult.GOOD;
		List<Triplet<Shot, Plan, Integer>> executedShotsList = level.executedShots;

		try {
			if (shotIndex + 1 <= executedShotsList.size()) {

				ABObject chosenTargetObject = level.currentScene.findObjectWithID(chosenPlan.getTargetObject());
				ABObject previousTargetObject = level.currentScene.findObjectWithID(executedShotsList.get(shotIndex).getTarget().getTargetObject());

				if (chosenTargetObject.equals(previousTargetObject)) {

					// If the shotToBeExecuted was the only shot in the list
					if (executedShotsList.size() == 1) {
						log.debug("Only shot in list, shot was rated as being optimal.");
						return resultEvaluation;
					} else if ((optimalShot(level) + calculateRatioPoints(level, shotIndex)) < 0.45) {
						log.debug("shot was bad in the first round: "
								+ (optimalShot(level) + calculateRatioPoints(level, shotIndex)) + ". Choosing another one...");
						resultEvaluation = EvaluationResult.BAD;
					}
				}
			}
		} catch (Exception e) {
			log.warn("Connection is lacking so no exact calculation possible.");
		}
		return resultEvaluation;
	}

	private static double optimalShot(Level level) {
		int totalBirdAmount = level.currentScene.getBirds().size() + level.executedShots.size();
		int usedBirds = level.executedShots.size();
		return (1 - (usedBirds / totalBirdAmount)) * 0.3;
	}

	private static double calculateRatioPoints(Level level, int shotIndex) {
		int numberOfBirds = level.currentScene.getBirds().size();
		int totalBirdAmount = numberOfBirds + level.executedShots.size();

		int estimatedShotPoints = level.getEstimatedMaximalPoints() - 10000 * (numberOfBirds - 1);
		log.debug("Damage Points of all shots: " + estimatedShotPoints);

		int averageReachablePoints = (estimatedShotPoints / totalBirdAmount);
		log.debug("Damage Points per shot: " + averageReachablePoints);

		int reachedPoints = level.executedShots.get(shotIndex).getDamage();

		return (reachedPoints / averageReachablePoints) * 0.7;
	}
}
