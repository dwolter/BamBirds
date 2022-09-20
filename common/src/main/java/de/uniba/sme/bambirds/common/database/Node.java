package de.uniba.sme.bambirds.common.database;

import de.uniba.sme.bambirds.common.Strategy;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.ShotHelper;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

public class Node {
	private static final Logger log = LogManager.getLogger(Node.class);

	enum NodeExecutionResult {
		WON,LOST,UNKNOWN
	}

	private Plan plan;
	private String unseenObject = "newObject";

	private int score = 0;
	private double confidence = 0;
	private boolean filtered = false;
	private NodeExecutionResult result = NodeExecutionResult.UNKNOWN;
	private ABType birdType;
	public boolean gameLost = false;
	public int tries = 0;

	/**
	 * The level in the Tree. 0 for the root node
	 */
	private int depthLevel = 0;

	/**
	 * The parent of this node. Null if it is the root node
	 */
	public Node parent = null;

	/**
	 * Some nodes lead to the same (or very similar) result. They are connected here to reduce the decision tree size
	 */ 
	private Node leadsTo = null;

	/**
	 * The children of this node.
	 */
	private List<Node> children = new ArrayList<>();
	public transient BufferedImage screenshot = null;
	private AbstractScene scene;

	private List<ABObject> abObjects = new ArrayList<>();
	private int affectedObj = 0;

	public Node(Plan plan) {
		this(plan, null);
	}

	public Node(Plan plan, AbstractScene scene) {
		this.plan = plan;
		this.scene = scene;
		if (plan != null) {
			this.confidence = plan.getConfidence();
			this.birdType = ABType.fromObjectID(plan.getBird());
		}
	}

	public double getConfidence() {
		return confidence;
	}

	public int getScoreDelta() {
		return parent.score - score;
	}

	public int getScore() {
		return score;
	}

	public void setScore(int score) {
		this.score = score;
	}

	public Shot getShot() {
		return plan.getShot();
	}
	
	public void setWon() {
		this.result = NodeExecutionResult.WON;
	}

	public void setLost() {
		this.result = NodeExecutionResult.LOST;
	}

	public boolean isLost() {
		return this.result  == NodeExecutionResult.LOST;
	}

	public boolean isWon() {
		return this.result == NodeExecutionResult.WON;
	}

	public boolean isExecuted() {
		return tries > 0;
	}

	public boolean isFiltered() {
		return filtered;
	}

	public void setFiltered(boolean filtered) {
		this.filtered = filtered;
	}

	public ABType getBirdType() {
		return birdType;
	}

	public void setLeadsTo(Node node) {
		this.leadsTo = node;
	}

	public Node getLeadsTo() {
		return leadsTo;
	}

	public synchronized List<Node> getChildren() {
		return new ArrayList<>(this.children);
	}

	public AbstractScene getScene() {
		return scene;
	}

	public AbstractScene getSceneBefore() {
		if (parent == null) {
			return null;
		}
		return parent.scene;
	}

	public void setScene(AbstractScene scene) {
		this.scene = scene;
	}

	public synchronized void setChildren(List<Node> children) {
		if (children == null)
			this.children = new ArrayList<>();
		else
			this.children = children;
	}

	public synchronized void markLostAndDecreaseConfidence(double confidenceMultiplier) {
		log.debug("Current node lost, propagating confidence penalty ...");
		setLost();
		decreaseConfidence(confidenceMultiplier, 1);
	}

	private synchronized void decreaseConfidence(double confidenceMultiplier, int exponent) {
		boolean childrenAllLost = false;
		boolean childrenHaveWon = false;
		for (Node children : getChildren()) {
			childrenAllLost &= children.isLost();
			childrenHaveWon |= children.isWon();
		}
		if (childrenAllLost && !childrenHaveWon) {
			setLost();
			confidence -= Math.pow(confidenceMultiplier, exponent);
			log.debug(String.format("New confidence: %.3f for %s", confidenceMultiplier, this));
			if (parent != null) {
				parent.decreaseConfidence(confidenceMultiplier, exponent + 1);
			}
		} else if (!childrenAllLost && childrenHaveWon) {
			setWon();
		}
	}

	public List<Node> getSiblings() {
		if (parent == null)
			return new ArrayList<>();
		return parent.getChildren();
	}

	public synchronized Node addChild(Node childNode) {
		childNode.parent = this;
		childNode.depthLevel = this.depthLevel + 1;
		children.add(childNode);
		return childNode;
	}

	public boolean containsChildWithShot(Shot s) {
		for (Node child : children) {
			if (child.equalShot(s))
				return true;
		}
		return false;
	}

	public Node getChildWithShot(Shot s) {
		for (Node child : children) {
			if (child.equalShot(s))
				return child;
		}
		return null;
	}

	public boolean equalShot(Shot shot) {
		log.trace("compare " + shot + " with " + plan.getShot());
		// FIXME: Find out if comparing to target IDs is actually wanted
		/*
		 * After impl of feedback ids hopefully maintained throughout shots; therefore
		 * use ids for equality and not angle.
		 */
		// return (Math.abs(savedShot.actualAngle - shot.actualAngle) < 0.001);
		return plan.getShot().equals(shot);
	}

	/**
	 * Compare screenshots for pixel difference
	 */
	public boolean equalScreenshot(BufferedImage img) {
		if (screenshot == null || img == null)
			return false;
		// TODO: add this method to a utility
		int pxDiff = ImageUtil.pixelDifference(screenshot, img, null,0);
		return (pxDiff < 1000); // 100x100 px area
	}

	public synchronized void printHierarchical(String prefix) {
		log.debug(prefix + this);
		for (Node child : children)
			child.printHierarchical(prefix + "  ");
	}

	@Override
	public String toString() {
		double ang = Double.NaN;
		int childCount = 0;

		if (plan != null)
			ang = plan.getReleaseAngle();
		if (children != null)
			childCount = children.size();

		return String.format("(Node @%.5f, confidence: %.3f, result: %s, children: %2d, depth: %d)", ang, confidence,
				result, childCount, depthLevel);
	}

	public void compareCurrentSituationWithPrevSituation() {
		if (this.parent == null) {
			return;
		}
		if (scene != null && scene.compareTo(this.parent.scene)) {
			log.debug("situation still looks the same ----------------");

			if (scene.birds.size() == this.parent.scene.birds.size()) {
				log.warn("Bird count is still the same, probably shot execution failed");
				// FIXME: when shot was actually not executed, this should be handled
			}

			if (!containsChildWithShot(this.plan.getShot())) {
				Node newNode = new Node(this.plan);
				// TODO: Why exactly is this done?
				newNode.setLost();
				this.addChild(newNode);
				log.debug("created lost copy of this shot");
			}
		}
	}

	public Plan getPlan() {
		return plan;
	}

	public synchronized List<Plan> getChildrenPlans() {
		return children.stream().map(Node::getPlan).collect(Collectors.toList());
	}

	/**
	 * Initialize child nodes list with available targets. Does not create
	 * duplicates
	 *
	 * @param planList Plan list from a {@link Strategy}
	 */
	public synchronized List<Node> createChildNodesFromTargetList(List<Plan> planList) {
		List<Node> newOrUpdatedChildNodes = new ArrayList<>();
		for (Plan plan : planList) {
			Node childNode = createChildNodeFromTarget(plan);
			if (childNode != null) {
				newOrUpdatedChildNodes.add(childNode);
			}
		}
		return newOrUpdatedChildNodes;
	}

	/**
	 * Create a child node from the given {@link Plan}. If the Plan is already
	 * present it is not added.
	 *
	 * @param plan Plan from a {@link Strategy}
	 */
	public synchronized Node createChildNodeFromTarget(Plan plan) {
		Shot shot = plan.getShot();
		if (shot != null) {
			return addOrUpdateChild(plan);
		}
		return null;
	}

	private synchronized Node addOrUpdateChild(Plan t) {
		Node childNode = getChildWithShot(t.getShot());
		if (childNode == null) {
			childNode = addChild(new Node(t));
		} else {
			// Only update node confidence if it is equal to the target confidence
			if (Double.compare(childNode.confidence, childNode.plan.getConfidence()) == 0) {
				childNode.confidence = t.getConfidence();
			}
			childNode.plan = t;
		}
		return childNode;
	}

	/**
	 * Update shot of this and all child nodes because of changes in the scaling factor
	 *
	 * @param currentScalingFactor the current scaling factor
	 * @param newScalingFactor     the new scaling factor
	 */
	public void adaptNodesToNewScalingFactor(double currentScalingFactor, double newScalingFactor) {
		AbstractScene siblingScene = scene != null ? scene : getSiblings().stream().filter(n -> n.scene != null).findFirst().orElse(this).getScene();
		if (plan != null && getShot() != null && siblingScene != null) {
			ABType birdType = ABType.fromObjectID(plan.getBird());
			if (birdType == ABType.Unknown)
				birdType = ABType.RedBird;
			ShotHelper.updateShotToNewScalingFactor(siblingScene.getSlingshot(), birdType, getShot(), currentScalingFactor, newScalingFactor);
		}
		children.forEach(n -> n.adaptNodesToNewScalingFactor(currentScalingFactor, newScalingFactor));
	}

	public void setResultingABObjects(List<ABObject> allObjects) {
		// Make deep copy of abObjects;
		// TODO: e.g. shape is not recognised
		log.info("set resulting abObjects size:  " + allObjects.size());
		this.abObjects = allObjects.stream().map(obj -> {
			ABObject copyObj = new ABObject(obj);
			copyObj.setGlobalID(obj.getGlobalID());
			return copyObj;
		}).collect(Collectors.toList());
		this.calcAffectedObjects();

	}

	/**
	 *
	 * @return How many objects have been significantly moved or deleted
	 */
	private int calcAffectedObjects() {

		if (parent == null || parent.abObjects == null || this.abObjects == null) {
			log.info("Caclulating affected Objects with null parent or null parent objects");
			// TODO: maybe change to debug?
			affectedObj = 0;
			return affectedObj;
		}

		affectedObj = parent.abObjects.stream()
				// Filter objects that are not anymore recognised by the mapper
				.filter(obj -> !unseenObject.equals(obj.getGlobalID())).map(previousObj -> {

					Optional<ABObject> successorObj = this.abObjects.stream()
							.filter(filterObj -> previousObj.getGlobalID().equals(filterObj.getGlobalID())).findFirst();

					if (successorObj.isPresent()) {
						Point2D previousPoint = previousObj.getCenter();
						Point2D successorPoint = successorObj.get().getCenter();

						// If the distance is greater than the threshold of 10 pixels, count obj as
						// affected
						// TODO: Find good threshold
						return successorPoint.distance(previousPoint) > 10d ? 1 : 0;
					} else {
						// Higher value because (probably this) object got destroyed
						return 3;
					}
				}).reduce(0, (a, b) -> a + b);
		return affectedObj;
	}

	public int getAffectedObj() {
		return affectedObj;
	}

	public List<ABObject> getABObjects() {
		return abObjects;
	}

	public void incrementTryCounter() {

		this.tries++;
	}

	public int getTried() {
		return tries;
	}

	public boolean wasTried() {
		return tries > 0;

	}
}