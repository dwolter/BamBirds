package de.uniba.sme.bambirds.planner.shot;

import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.*;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;

import de.uniba.sme.bambirds.common.utils.SelectionAlgorithms;
import de.uniba.sme.bambirds.common.utils.Settings;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ShotSelection implements MetaShotSelection {
	private static final Logger log = LogManager.getLogger(ShotSelection.class);

	// private shot shot;
	private Level currentLevel;
	private int executedShots;
	public boolean fallbackToDemoShot = true;

	private final Map<Node,Double> possibleShots = new HashMap<>();
	private Plan chosenPlan = null;
	private final Random random = new Random();
	private Shot shotToVary = null;
	private int removedShotCounter;

	public ShotSelection(Level currentLevel, int shotNumber) {
		this.currentLevel = currentLevel;
		this.executedShots = shotNumber;
	}

	public int getRemovedShotCounter(){
		return removedShotCounter;
	}

	public Plan getChosenTarget() { return chosenPlan; }

	@Override
	public boolean generateAndEvaluateShotOptions(List<Plan> plans) {
		shotToVary = null;
		
		if (plans == null){
			return false;
		} else if (plans.isEmpty() && currentLevel.tree.getCurrentNode().getChildren().size() == 0) {
			Plan demoPlan = getDemoPlan();
			if (demoPlan == null) {
				return false;
			}
			plans.add(demoPlan);
		}

		fallbackToDemoShot = false;
		currentLevel.tree.getCurrentNode().createChildNodesFromTargetList(plans);

		return generateAndEvaluateShotOptions();
	}

	private double calculateScore(List<ShotEffect> effects) {
		return calculateScore(
				(int) effects.stream().filter(e -> e.getType() == ShotEffect.EffectType.DESTROY).count(),
				(int) effects.stream().filter(e -> e.getType() == ShotEffect.EffectType.MOVE).count(),
				(int) effects.stream().filter(e -> e.getType() == ShotEffect.EffectType.FREE).count()
		);
	}

	private double calculateScore(int countDestroyedPigs, int countAffectedPigs, int countFreedPigs) {
		return countDestroyedPigs
				+ countAffectedPigs * 0.5
				+ countFreedPigs * 0.75;
	}

	@Override
	public boolean generateAndEvaluateShotOptions() {
		fallbackToDemoShot = false;
		try {
			List<Node> childNodes = currentLevel.tree.getCurrentNode().getChildren();
			if (childNodes == null || childNodes.isEmpty())
				throw new NullPointerException("No targets");

			List<Node> bestChilds = childNodes.stream()
				.filter(node -> !node.isLost() || !node.isFiltered())
				.collect(Collectors.toList());

			log.debug("child nodes before: "+childNodes.size()+" and after filtering: "+bestChilds.size());
			if (bestChilds.isEmpty()) {
				List<Node> filteredChilds = childNodes.stream()
					.filter(node -> !node.isLost() && node.isFiltered())
					.collect(Collectors.toList());
				if (!filteredChilds.isEmpty()) {
					// Set best Childs to all filtered and not lost childs
					bestChilds = filteredChilds;
				} else {
					// Select a shot from the lost childs 
					List<Node> lostChilds = childNodes.stream()
						.filter(node -> node.isLost())
						.collect(Collectors.toList());
					shotToVary = lostChilds.get(random.nextInt(lostChilds.size())).getShot();
					throw new NullPointerException("All nodes lead to failure");
				}
			}
			Set<ABObject> priorityPigs = currentLevel.tree.getPriorityPigs();
			bestChilds.forEach(n -> {
				// TODO: When all nodes have confidence less than 0 this needs to be redone
				if (n.getConfidence() < 0) return;
				Optional<ExecutedNode> mostProbableNodeMatch = currentLevel.tree
						.getExecutedNodes().stream()
						.max(Comparator.comparingDouble(executedNode -> executedNode.similarityTo(n)));
				double caseBasedScore = 0;
				double caseBasedProbability = 0;
				if (mostProbableNodeMatch.isPresent()) {
					ExecutedNode matchedNode = mostProbableNodeMatch.get();
					caseBasedProbability = matchedNode.similarityTo(n);
					caseBasedScore = calculateScore(matchedNode.getEffects());
				}
				Plan p = n.getPlan();
				Double score = calculateScore(p.getDestroyedPigs().size(), p.getAffectedPigs().size(), p.getFreedPigs().size())
						* ( (1-caseBasedProbability) * n.getConfidence())
						+ caseBasedProbability * caseBasedScore;
				List<ShotEffect> effects = p.getExpectedEffects(currentLevel.currentScene);
				int priorityCalc = effects.stream().mapToInt(shotEffect -> {
					if (priorityPigs.contains(shotEffect.getObject())) {
						switch (shotEffect.getType()) {
							case FREE:
								return 2;
							case MOVE:
								return 3;
							case DESTROY:
								return 4;
							default:
								return 1;
						}
					}
					return 1;
				}).reduce(1,(a,b) -> a*b);
				possibleShots.put(n, score * priorityCalc);
			});
			bestChilds.sort(Comparator.comparingDouble(n -> -n.getConfidence()));
		}
		catch (Exception e) {
			fallbackToDemoShot = true;
			log.error("Error generating Targets, falling back to demo shot.", e);
		}
		return !fallbackToDemoShot;
	}

	private boolean isNotAnAlreadyFailedShot(Node childNode){
		return true;
	}

	public Node mostPromisingNode() {
		Node selectedNode;

		if (fallbackToDemoShot || possibleShots == null || possibleShots.isEmpty()){
			Plan demoPlan = getDemoPlan();
			if (demoPlan == null) {
				return null;
			}
			selectedNode = new Node(demoPlan);
			currentLevel.tree.getCurrentNode().addChild(selectedNode);
		} else {
			// author: Daniel Lutalo - Agent X
			// TODO: Improve selection propability 
			// - DONE: Put not just confidence but rather combination of confidence and gain (gain defined as number of pigs expected to be destroyed)
			// - Change confidence based on similarity to previously executed shots and their evaluation result
			Map<Node, Double> normalizedActions = SelectionAlgorithms.secondOrderThompsonSample(possibleShots,Settings.SHOTSELECTION_EXPONENT);
			selectedNode = SelectionAlgorithms.randomWeighted(normalizedActions);
		}
		chosenPlan = selectedNode.getPlan();
		currentLevel.tree.setCurrentNode(selectedNode);

		// do we really care if the object can not be found in scene?
		// ABObject chosenABTarget =
		// currentLevel.currentScene.findObjectWithID(chosenTarget.getTargetId());
		// if (chosenABTarget == null)
		// throw new NullPointerException("Could not find target in scene");

		log.info("Chosen target: " + chosenPlan);
		return selectedNode;
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


	public Plan getDemoPlan() {
		TrajectoryPlanner tp = new TrajectoryPlanner();
		Rectangle sling = currentLevel.currentScene.getSlingshot();
		ABType birdOnSling = currentLevel.currentScene.getBirdTypeOnSling();
		List<ABObject> pigs = currentLevel.currentScene.getPigs();
		if (sling != null) {

			if (!pigs.isEmpty() && shotToVary == null) {

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
						log.debug("Release Point: " + releasePoint);
						log.debug("Release Angle: " + Math.toDegrees(releaseAngle));
						int tapInterval;
						switch (birdOnSling) {
						case RedBird:
							tapInterval = 0;
							break;
						case YellowBird:
							tapInterval = 65 + random.nextInt(25);
							break;
						case WhiteBird:
							tapInterval = 70 + random.nextInt(15);
							break;
						case BlackBird:
							tapInterval = 90 + random.nextInt(25);
							break;
						case BlueBird:
							tapInterval = 65 + random.nextInt(20);
							break;
						default:
							tapInterval = 60;
						}

						int tapTime = tp.getTapTime(sling, releasePoint, _tpt, tapInterval);
						dx = (int) releasePoint.getX() - refPoint.x - 50 + random.nextInt(100);
						dy = (int) releasePoint.getY() - refPoint.y - 50 + random.nextInt(100);
						//TODO: Calculate angles and Parabola correctly
						return new Plan(pig.getGlobalID(), 42, "demo", 0,
								new Shot(refPoint.x, refPoint.y, dx, dy, _tpt.x, _tpt.y, 0, tapTime),
								ThinkerType.DEMO);
					} else {
						log.error("No Release Point Found");
						return null;
					}
				}
			} else if (shotToVary != null) {
				int dx = shotToVary.getDragX() - 80 + random.nextInt(160);
				int dy = shotToVary.getDragY() - 80 + random.nextInt(160);
				int tap = (int) shotToVary.getTapTime() - 200 + random.nextInt(400);
				//TODO: Calculate angles and Parabola correctly
				return new Plan("random", 42, "variedShot", 0,
						new Shot(shotToVary.getSlingX(), shotToVary.getSlingY(), dx, dy, shotToVary.getTargetX(), shotToVary.getTargetY(), 0, tap),
						ThinkerType.DEMO);
			}
		}
		return null;
	}

	public List<Node> getAvailableTargets() {
		return new ArrayList<>(this.possibleShots.keySet());
	}

	public void printAvailableTargets() {
		log.debug("");
		log.debug("Available targets: ");
		if (fallbackToDemoShot) {
			log.debug("-- none --");
		}
		else {
			log.debug(Integer.toString(possibleShots.size()));
			for (Map.Entry<Node,Double> n : possibleShots.entrySet().stream().sorted(Comparator.comparingDouble(Map.Entry::getValue)).collect(Collectors.toList())) {
				if (n.getKey().getPlan() != null)
					log.debug(String.format("Score: %4.1f %s", n.getValue(), n.getKey().getPlan().prettyPrint()));
			}
		}
	}

	@Override public String toString() {
		return String.format("(ShotSelection useDemoShot: %b, targets: %d, chosen: %s)",
				fallbackToDemoShot, possibleShots.size(), chosenPlan);
	}

	public boolean isFallbackToDemoShot() {
		return fallbackToDemoShot;
	}

}
