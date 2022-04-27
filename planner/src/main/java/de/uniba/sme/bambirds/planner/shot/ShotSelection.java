package de.uniba.sme.bambirds.planner.shot;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.objects.Node;
import de.uniba.sme.bambirds.common.objects.Target;
import de.uniba.sme.bambirds.common.objects.Triplet;
import de.uniba.sme.bambirds.common.objects.SavedShot;
import de.uniba.sme.bambirds.common.objects.Shot;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ShotSelection {
	private static final Logger log = LogManager.getLogger(ShotSelection.class);
	public enum ShotEvaluation {
		GOOD, BAD
	}

	// private shot shot;
	private Level currentLevel;
	private int executedShots;
	public boolean fallbackToDemoShot = true;

	private List<Node> filteredNodeList = null;
	private Target chosenTarget = null;
	private Random random = new Random();
    private Shot shotToVary = null;

	public ShotSelection(Level currentLevel, int shotNumber) {
		this.currentLevel = currentLevel;
		this.executedShots = shotNumber;
	}

	public Target getChosenTarget() { return chosenTarget; }

	public boolean generateAndEvaluateShotOptions(List<Target> targets, List<SavedShot> savedShots) {
		shotToVary = null;
		
		if (targets == null || targets.isEmpty())
			return false;

		fallbackToDemoShot = false;
		currentLevel.tree.createChildNodesFromTargetList(targets, savedShots);

		try {
			ArrayList<Node> childNodes = currentLevel.tree.getCurrentNode().getChildren();
			if (childNodes == null || childNodes.isEmpty())
				throw new NullPointerException("No targets");

			List<Node> childsNotLost = new ArrayList<>();
			for (Node childNode : childNodes){
				if (!childNode.gameLost){
					childsNotLost.add(childNode);
				}
			}
			log.info("child nodes before: "+childNodes.size()+" and after removal: "+childsNotLost.size());
			if (childsNotLost.isEmpty()) {
				shotToVary = childNodes.get(random.nextInt(childNodes.size())).getShot();
				throw new NullPointerException("All nodes lead to failure");
			}

			childsNotLost.sort(Comparator.comparingDouble(n -> -n.getConfidence()));
			filteredNodeList = childsNotLost;
			return true;
		}
		catch (Exception e) {
			fallbackToDemoShot = true;
			log.error("[ShotSelection] Error generating Targets: " + e.getMessage()
					+ ", falling back to demo shot.");
		}
		return false;
	}

	public Shot mostPromisingShot() {
		if (fallbackToDemoShot || filteredNodeList == null || filteredNodeList.isEmpty())
			return getDemoShot();

		// Author of the following:
		// Daniel Lutalo - Agent X

		// hyperparameter further filtering available nodes, removes nodes not at least 'X' percent of the highest evaluated confidence
		double percentageOfMaxScore = 0.8;

		// hyperparameter tuning exploration vs exploitation, exponentiates then renormalises the weights for a probability distribution
		double exponent = 10;

		List<Node> furtherFilteredNodeList = filterNodesByScore(filteredNodeList, filteredNodeList.get(0).getConfidence() * percentageOfMaxScore);
		Map<Node,Double> actions = new HashMap<Node,Double>();
		furtherFilteredNodeList.forEach(n -> actions.put(n, n.getConfidence()));
		TreeMap<Double,Node> weightedActions = new TreeMap<>();
		double cumulativeProbability = 0;
        for (Map.Entry<Node,Double> kv: secondOrderThompsonSample(actions,exponent).entrySet()){
            weightedActions.put(cumulativeProbability += kv.getValue(), kv.getKey());   
        }

		Node highestConfidenceNode = weightedActions.higherEntry(Math.random()).getValue();
		chosenTarget = highestConfidenceNode.target;
		currentLevel.tree.setCurrentNode(highestConfidenceNode);

		// do we really care if the object can not be found in scene?
//		ABObject chosenABTarget = currentLevel.currentScene.findObjectWithID(chosenTarget.getTargetId());
//		if (chosenABTarget == null)
//			throw new NullPointerException("Could not find target in scene");

		log.info("Chosen target: " + chosenTarget);
		return highestConfidenceNode.getShot();
	}

	/**
	 * Filters the available nodes and only keeps ones with a confidence level above a given threshold 
	 * @param nodeList the list of Nodes to be filtered
	 * @param minimumScoreRequired the score threshold used to filter the list of Nodes
	 * @return a list of Nodes with scores >= to the minimum threshold
	 * @author Daniel Lutalo - Agent X
	 */
	private List<Node> filterNodesByScore(List<Node> nodeList, double minimumScoreRequired){
		List<Node> filteredList = new ArrayList<Node>();
		nodeList.forEach(n -> {if (n.getConfidence() >= minimumScoreRequired) {filteredList.add(n);}});
		return filteredList;
		// return nodeList.stream().filter(n -> n.getConfidence() >= minimumScoreRequired).collect(Collectors.toList());
	}

	/**
	 * My new method called Second Order Thompson Sampling with exponent k in [0,infinity)
	 * (you could also make k negative if you want 'negative exploitation', i.e. a higher probability of selecting lower evaluated actions)
	 * vary k to select a custom trade-off between exploration vs exploitation
	 * k = 1 -> regular Thompson sampling - linear
	 * k > 1 -> approaches greedy selection as k approaches infinity (pure exploitation) - superlinear
	 * k < 1 -> approaches a uniform sample as k approaches 0 (pure exploration) - sublinear
	 * @param originalProbabilities
	 * @param exponent
	 * @return
	 * @author Daniel Lutalo - Agent X
	 */
	private Map<Node, Double> secondOrderThompsonSample(Map<Node, Double> originalProbabilities, double exponent){
		// normalise scores into probabilities (first Thompson sample)
		double sum = originalProbabilities.values().stream().mapToDouble(v->v).reduce(0, Double::sum);
		Map<Node, Double> normalisedProbabilities = new HashMap<Node, Double>();
		originalProbabilities.forEach((k,v) -> normalisedProbabilities.put(k,v/sum));

		// exponentiate the probabilities and renormalise (second Thompson sample)
		double exponentiatedSum = normalisedProbabilities.values().stream().mapToDouble(v->v).reduce(0, (a,b) -> a + Math.pow(b, exponent));
		Map<Node, Double> finalProbabilities = new HashMap<Node, Double>();
		normalisedProbabilities.forEach((k,v) -> finalProbabilities.put(k,v/exponentiatedSum));

		return finalProbabilities;
	}

	public Shot getDemoShot() {
		TrajectoryPlanner tp = new TrajectoryPlanner();
		Rectangle sling = currentLevel.currentScene.getSlingshot();
		ABType birdOnSling = currentLevel.currentScene.getBirdTypeOnSling();
		List<ABObject> pigs = currentLevel.currentScene.getPigs();
		if (sling != null) {

			if (!pigs.isEmpty() && shotToVary==null) {

				Point releasePoint = null;
				int dx, dy;
				{
					ABObject pig = pigs.get(random.nextInt(pigs.size()));

					Point _tpt = pig.getCenter();
					ArrayList<Point> pts = tp.estimateLaunchPoint(sling, _tpt);

					if (pts.size() == 1)
						releasePoint = pts.get(0);
					else if (pts.size() == 2) {
						if (random.nextInt(6) == 0)
							releasePoint = pts.get(1);
						else
							releasePoint = pts.get(0);
					} else if (pts.isEmpty()) {
						log.warn("No release point found for the target");
						log.info("Try a shot with 45 degree");
						releasePoint = TrajectoryPlanner.findReleasePoint(sling, Math.PI / 4);
					}
					Point refPoint = TrajectoryPlanner.getReferencePoint(sling);
					if (releasePoint != null) {
						double releaseAngle = tp.getReleaseAngle(sling, releasePoint);
						log.info("Release Point: " + releasePoint);
						log.info("Release Angle: " + Math.toDegrees(releaseAngle));
						int tapInterval;
						switch (birdOnSling) {
							case RedBird:    tapInterval = 0; break;
							case YellowBird: tapInterval = 65 + random.nextInt(25); break;
							case WhiteBird:  tapInterval = 70 + random.nextInt(15); break;
							case BlackBird:  tapInterval = 90 + random.nextInt(25); break;
							case BlueBird:   tapInterval = 65 + random.nextInt(20); break;
							default:         tapInterval = 60;
						}

						int tapTime = tp.getTapTime(sling, releasePoint, _tpt, tapInterval);
						dx = (int) releasePoint.getX() - refPoint.x - 50 + random.nextInt(100);
						dy = (int) releasePoint.getY() - refPoint.y - 50 + random.nextInt(100);
						return new Shot(refPoint.x, refPoint.y, dx, dy, 0, tapTime);
					} else {
						log.error("No Release Point Found");
						return null;
					}
				}
			} else if (shotToVary != null) {
				int dx = shotToVary.getDx() - 80 + random.nextInt(160);
				int dy = shotToVary.getDy() - 80 + random.nextInt(160);
				int tap = (int) shotToVary.getT_tap() - 200 + random.nextInt(400);
				return new Shot(shotToVary.getX(), shotToVary.getY(), dx, dy, 0, tap);
			}
		}
		return null;
	}

	private ShotEvaluation evaluate(Target chosenTarget, Level levelInStore) {
		ShotEvaluation resultEvaluation = ShotEvaluation.GOOD;
		List<Triplet<Shot, Target, Integer>> executedShotsList = levelInStore.executedShots;

		try {
			if (executedShots + 1 <= executedShotsList.size()) {

				ABObject chosenTargetObject = currentLevel.currentScene.findObjectWithID(chosenTarget.getTargetId());
				ABObject previousTargetObject = currentLevel.currentScene.findObjectWithID(executedShotsList.get(executedShots).getTarget().getTargetId());

				if (chosenTargetObject.equals(previousTargetObject)) {

					// If the shotToBeExecuted was the only shot in the list
					if (executedShotsList.size() == 1) {
						log.info("[Meta] Only shot in list, shot was rated as being optimal.");
						return resultEvaluation;
					} else if ((optimalShot() + calculateRatioPoints(levelInStore)) < 0.45) {
						log.info("[Meta] shot was bad in the first round: "
								+ (optimalShot() + calculateRatioPoints(levelInStore)) + ". Choosing another one...");
						resultEvaluation = ShotEvaluation.BAD;
					}
				}
			}
		} catch (Exception e) {
			log.warn("[Meta] Connection is lacking so no exact calculation possible.");
		}
		return resultEvaluation;
	}

	private double optimalShot() {
		int totalBirdAmount = currentLevel.currentScene.getBirds().size() + currentLevel.executedShots.size();
		int usedBirds = currentLevel.executedShots.size();
		return (1 - (usedBirds / totalBirdAmount)) * 0.3;
	}

	private double calculateRatioPoints(Level levelInStore) {
		int numberOfBirds = currentLevel.currentScene.getBirds().size();
		int totalBirdAmount = numberOfBirds + currentLevel.executedShots.size();

		int estimatedShotPoints = currentLevel.getEstimatedMaximalPoints() - 10000 * (numberOfBirds - 1);
		log.info("Damage Points of all shots: " + estimatedShotPoints);

		int averageReachablePoints = (estimatedShotPoints / totalBirdAmount);
		log.info("Damage Points per shot: " + averageReachablePoints);

		int reachedPoints = levelInStore.executedShots.get(executedShots).getDamage();

		return (reachedPoints / averageReachablePoints) * 0.7;
	}

	public List<Node> getAvailableTargets() {
		if(this.filteredNodeList == null) {
			return new ArrayList<Node>();
		}
		return this.filteredNodeList;
	}

	public void printAvailableTargets() {
		log.info("");
		log.info("Available targets: ");
		if (fallbackToDemoShot) {
			log.info("-- none --");
		}
		else if (filteredNodeList != null) {
			log.info(Integer.toString(filteredNodeList.size()));
			for (Node n : filteredNodeList) {
				if (n.target != null)
					log.info(n.target.prettyPrint());
			}
		}
	}

	@Override public String toString() {
		return String.format("(ShotSelection useDemoShot: %b, targets: %d, chosen: %s)",
				fallbackToDemoShot, (filteredNodeList == null ? 0 : filteredNodeList.size()), chosenTarget);
	}
}
