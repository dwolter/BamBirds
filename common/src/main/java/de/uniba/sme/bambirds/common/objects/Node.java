package de.uniba.sme.bambirds.common.objects;

import de.uniba.sme.bambirds.common.Strategy;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.ImageUtil;

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import de.uniba.sme.bambirds.common.utils.ShotHelper;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Node {
	private static final Logger log = LogManager.getLogger(Node.class);

	public Plan plan;

	public int points = 0;
	private double confidence = 0;
	public boolean gameLost = false;

	private int depthLevel = 0;
	public Node parent = null;
	private List<Node> children = new ArrayList<>();
	public transient BufferedImage screenshot = null;
	public List<Point> pointList = new ArrayList<>();
	private AbstractScene scene;

	public Node(Plan plan) {
		this(plan, null);
	}

	public Node(Plan plan, AbstractScene scene) {
		this.plan = plan;
		this.scene = scene;
		if (plan != null)
			this.confidence = plan.getConfidence();
	}

	public double getConfidence() {
		return confidence;
	}

	public int getPoints() {
		return points;
	}

	public void setPoints(int points) {
		this.points = points;
	}

	public Shot getShot() {
		return plan.getShot();
	}

	public synchronized List<Node> getChildren() {
		return new ArrayList<>(this.children);
	}

	public AbstractScene getScene() {
		return scene;
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
		this.gameLost = true;
		decreaseConfidence(confidenceMultiplier, 1);
	}

	private synchronized void decreaseConfidence(double confidenceMultiplier, int exponent) {
		for (Node sibling : getChildren()) {
			gameLost &= sibling.gameLost;
		}
		confidence -= Math.pow(confidenceMultiplier, exponent);
		log.debug(String.format("New confidence: %.3f for %s", confidenceMultiplier, this));
		if (parent != null) {
			parent.decreaseConfidence(confidenceMultiplier, exponent+1);
		}
	}

	public List<Node> getSiblings() {
		if (parent == null)
			return new ArrayList<>();
		return parent.getChildren();
	}

	public synchronized void addChild(Node childNode) {
		childNode.parent = this;
		childNode.depthLevel = this.depthLevel + 1;
		children.add(childNode);
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
		return plan.getShot().equals(shot);
	}

	/** Compare screenshots for pixel difference */
	public boolean equalScreenshot(BufferedImage img) {
		if (screenshot == null || img == null)
			return false;
		// TODO: add this method to a utility
		int pxDiff = ImageUtil.pixelDifference(screenshot, img);
		return (pxDiff < 10000); // 100x100 px area
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
			ang = plan.getImpactAngle();
		if (children != null)
			childCount = children.size();

		return String.format("(Node @%.5f, confidence: %.3f, lost: %b, children: %2d, depth: %d)", ang, confidence,
				gameLost, childCount, depthLevel);
	}

	public void compareCurrentSituationWithPrevSituation() {
		if (this.parent == null || this.parent.pointList == null) {
			return;
		}
		if (scene != null && scene.compareTo(this.parent.scene)) {
			log.debug("situation still looks the same ----------------");

			if (!containsChildWithShot(this.plan.getShot())) {
				Node newNode = new Node(this.plan);
				// TODO: Why exactly is this done?
				newNode.gameLost = true;
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
	 * @param planList Target list from a {@link Strategy}
	 */
	public synchronized void createChildNodesFromTargetList(List<Plan> planList) {
		for (Plan plan : planList) {
			createChildNodeFromTarget(plan);
		}
	}

	/**
	 * Create a child node from the given {@link Plan}. If the Plan is already
	 * present it is not added.
	 * 
	 * @param plan Plan from a {@link Strategy}
	 */
	public synchronized void createChildNodeFromTarget(Plan plan) {
		Shot shot = plan.getShot();
		if (shot != null) {
			try {
				addOrUpdateChild(plan);
			} catch (IllegalArgumentException e) {
				log.error("failed to parse special shot", e);
			}
		}
	}

	private synchronized void addOrUpdateChild(Plan t) {
		Node childNode = getChildWithShot(t.getShot());
		if (childNode == null) {
			addChild(new Node(t));
		} else if (t.getConfidence() >= 0 && childNode.plan.getConfidence() < 0) {
			// Only update node confidence if it is equal to the target confidence
			if (Double.compare(childNode.confidence, childNode.plan.getConfidence()) == 0) {
				childNode.confidence = t.getConfidence();
			}
			childNode.plan = t;
		}
	}

	/**
	 * Update shot of this and all child nodes because of changes in the scaling factor
	 * @param currentScalingFactor the current scaling factor
	 * @param newScalingFactor the new scaling factor
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
}