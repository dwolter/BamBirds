package de.uniba.sme.bambirds.planner.shot;

import java.util.List;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.database.Node;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.Triplet;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;

public class ShotEvaluation {
	private static final Logger log = LogManager.getLogger();

  public enum EvaluationResult {
		PHENOMENAL,
    GOOD,
		MEDIUM,
    BAD,
		UNKNOWN
  }
  
	/**
	 * Evaluate a node based on the resulting scene.
	 * Should be executed after the scene is stable, 
	 * but the results are not necesseary until the next level playthrough.
	 * It also adds an ExecutedNode to the DecisionTree with the ShotEffects.
	 * 
	 * @return How good the executed shot was based on it's .
	 */
	public static EvaluationResult evaluate(Node executedNode, Level level, int shotIndex) {

		AbstractScene currScene = executedNode.getScene();
		AbstractScene prevScene = executedNode.parent.getScene();

		// did shot destroy anything?

		if (prevScene.compareTo(currScene)) {
			return EvaluationResult.BAD;
		}

		// did shot destroy the amount of pigs it claimed? 

		int expectedDestroyedPigs = executedNode.getPlan().getDestroyedPigs().size();
		int actuallyDestroyedPigs = prevScene.getPigs().size() - currScene.getPigs().size();

		EvaluationResult result = EvaluationResult.GOOD;
		
		if (actuallyDestroyedPigs < 0) {
			log.warn("Pig count bigger than before, probably some artifact...");
			// FIXME: What to do in that case?
			result = EvaluationResult.UNKNOWN;
		} else if (actuallyDestroyedPigs == 0) {
			// FIXME: what about clearing path plans?
			result = EvaluationResult.BAD;
		} else if (expectedDestroyedPigs > actuallyDestroyedPigs) {
			result = EvaluationResult.MEDIUM;
		} else if (expectedDestroyedPigs < actuallyDestroyedPigs) {
			result = EvaluationResult.PHENOMENAL;
		}

		return result;



		// EvaluationResult resultEvaluation = EvaluationResult.GOOD;
		// List<Node> executedNodes = level.executedNodes;

		// try {
		// 	if (shotIndex + 1 <= executedNodes.size()) {

		// 		ABObject chosenTargetObject = level.currentScene.findObjectWithID(executedNode.getPlan().getTargetObject());
		// 		ABObject previousTargetObject = level.currentScene.findObjectWithID(executedNodes.get(shotIndex).getPlan().getTargetObject());

		// 		if (chosenTargetObject.equals(previousTargetObject)) {

		// 			// If the shotToBeExecuted was the only shot in the list
		// 			if (executedNodes.size() == 1) {
		// 				log.debug("Only shot in list, shot was rated as being optimal.");
		// 				return resultEvaluation;
		// 			} else if ((optimalShot(level) + calculateRatioPoints(level, shotIndex)) < 0.45) {
		// 				log.debug("shot was bad in the first round: "
		// 						+ (optimalShot(level) + calculateRatioPoints(level, shotIndex)) + ". Choosing another one...");
		// 				resultEvaluation = EvaluationResult.BAD;
		// 			}
		// 		}
		// 	}
		// } catch (Exception e) {
		// 	log.warn("Connection is lacking so no exact calculation possible.");
		// }
		// return resultEvaluation;
	}

	private static double optimalShot(Level level) {
		int totalBirdAmount = level.currentScene.getBirds().size() + level.executedNodes.size();
		int usedBirds = level.executedNodes.size();
		return (1 - (usedBirds / totalBirdAmount)) * 0.3;
	}

	private static double calculateRatioPoints(Level level, int shotIndex) {
		int numberOfBirds = level.currentScene.getBirds().size();
		int totalBirdAmount = numberOfBirds + level.executedNodes.size();

		int estimatedShotPoints = level.getEstimatedMaximalPoints() - 10000 * (numberOfBirds - 1);
		log.debug("Damage Points of all shots: " + estimatedShotPoints);

		int averageReachablePoints = (estimatedShotPoints / totalBirdAmount);
		log.debug("Damage Points per shot: " + averageReachablePoints);

		int reachedPoints = level.executedNodes.get(shotIndex).getScoreDelta();

		return (reachedPoints / averageReachablePoints) * 0.7;
	}
}
