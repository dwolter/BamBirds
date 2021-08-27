package de.uniba.sme.bambirds.planner.shot;

import de.uniba.sme.bambirds.common.objects.*;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.Plan;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.*;

import de.uniba.sme.bambirds.common.utils.SelectionAlgorithms;
import de.uniba.sme.bambirds.common.utils.Settings;
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
	private Plan chosenPlan = null;
	private final Random random = new Random();
	private Shot shotToVary = null;

	public ShotSelection(Level currentLevel, int shotNumber) {
		this.currentLevel = currentLevel;
		this.executedShots = shotNumber;
	}

	public Plan getChosenTarget() { return chosenPlan; }

	public boolean generateAndEvaluateShotOptions(List<Plan> plans) {
		shotToVary = null;
		
		if (plans == null){
			return false;
		} else if (plans.isEmpty()) {
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

	public boolean generateAndEvaluateShotOptions() {
		fallbackToDemoShot = false;
		try {
			List<Node> childNodes = currentLevel.tree.getCurrentNode().getChildren();
			if (childNodes == null || childNodes.isEmpty())
				throw new NullPointerException("No targets");

			List<Node> childsNotLost = new ArrayList<>();
			for (Node childNode : childNodes){
				if (!childNode.gameLost){
					childsNotLost.add(childNode);
				}
			}
			log.debug("child nodes before: "+childNodes.size()+" and after removal: "+childsNotLost.size());
			if (childsNotLost.isEmpty()) {
				shotToVary = childNodes.get(random.nextInt(childNodes.size())).getShot();
				throw new NullPointerException("All nodes lead to failure");
			}

			childsNotLost.sort(Comparator.comparingDouble(n -> -n.getConfidence()));
			filteredNodeList = childsNotLost;
		}
		catch (Exception e) {
			fallbackToDemoShot = true;
			log.error("Error generating Targets, falling back to demo shot.", e);
		}
		return !fallbackToDemoShot;
	}

	public Shot mostPromisingShot() {
		Node selectedNode;

		if (fallbackToDemoShot || filteredNodeList == null || filteredNodeList.isEmpty()){
			Plan demoPlan = getDemoPlan();
			if (demoPlan == null) {
				return null;
			}
			selectedNode = new Node(demoPlan);
			currentLevel.tree.getCurrentNode().addChild(selectedNode);
		} else {
			Map<Node, Double> map = new HashMap<>();
			filteredNodeList.forEach(n -> map.put(n, n.getConfidence()));
			selectedNode = SelectionAlgorithms.epsilonGreedy(SelectionAlgorithms.softmax(map), Settings.NODE_SELECTION_EPSILON);
		}

		chosenPlan = selectedNode.plan;
		currentLevel.tree.setCurrentNode(selectedNode);

		// do we really care if the object can not be found in scene?
//		ABObject chosenABTarget = currentLevel.currentScene.findObjectWithID(chosenTarget.getTargetId());
//		if (chosenABTarget == null)
//			throw new NullPointerException("Could not find target in scene");

		log.info("Chosen target: " + chosenPlan);
		return selectedNode.getShot();
	}

	/**
	 * Generate a demo plan. A new demoShot is generated at a random target
	 * @return Target With the shot description
	 */
	public Plan getDemoPlan() {
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
						log.debug("Release Point: " + releasePoint);
						log.debug("Release Angle: " + Math.toDegrees(releaseAngle));
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
						//TODO: Calculate angles and Parabola correctly
						return new Plan(pig.globalID, 42, "demo", 0,
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
				return new Plan("random", 42, "demo", 0,
						new Shot(shotToVary.getSlingX(), shotToVary.getSlingY(), dx, dy, shotToVary.getTargetX(), shotToVary.getTargetY(), 0, tap),
						ThinkerType.DEMO);
			}
		}
		return null;
	}

	public List<Node> getAvailableTargets() {
		if(this.filteredNodeList == null) {
			return new ArrayList<Node>();
		}
		return this.filteredNodeList;
	}

	public void printAvailableTargets() {
		log.debug("");
		log.debug("Available targets: ");
		if (fallbackToDemoShot) {
			log.debug("-- none --");
		}
		else if (filteredNodeList != null) {
			log.debug(Integer.toString(filteredNodeList.size()));
			for (Node n : filteredNodeList) {
				if (n.plan != null)
					log.debug(n.plan.prettyPrint());
			}
		}
	}

	@Override public String toString() {
		return String.format("(ShotSelection useDemoShot: %b, targets: %d, chosen: %s)",
				fallbackToDemoShot, (filteredNodeList == null ? 0 : filteredNodeList.size()), chosenPlan);
	}
}
