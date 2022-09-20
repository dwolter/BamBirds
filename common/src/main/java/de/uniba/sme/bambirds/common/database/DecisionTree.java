package de.uniba.sme.bambirds.common.database;

import de.uniba.sme.bambirds.common.objects.ExecutedNode;
import de.uniba.sme.bambirds.common.objects.GameState;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

/**
 * a class to represent a tree of possible choices for Shots.
 */
public class DecisionTree {
	private static final Logger LOG = LogManager.getLogger(DecisionTree.class);
	private final Node root;
	private Node currentNode;
	private final Set<ExecutedNode> executedNodes;
	private final Set<ABObject> priorityPigs;

	public DecisionTree() {
		root = new Node(null);
		setCurrentNode(root);
		executedNodes = new TreeSet<>();
		priorityPigs = new TreeSet<>();
	}

	public void resetToRootNode() {
		if (this.currentNode != this.root) {
			this.currentNode.setLost();
			this.currentNode = this.root;
		}
	}

	public Node getCurrentNode() {
		return currentNode;
	}

	public void setCurrentNode(Node currentNode) {
		this.currentNode = currentNode;
	}

	public void addExecutedNode(ExecutedNode executedNode) {
		this.executedNodes.add(executedNode);
	}

	public Set<ExecutedNode> getExecutedNodes() {
		return Collections.unmodifiableSet(executedNodes);
	}

	public void addPriorityPigs(Collection<ABObject> pigs) {
		this.priorityPigs.addAll(pigs);
	}

	public Set<ABObject> getPriorityPigs() {
		return Collections.unmodifiableSet(priorityPigs);
	}

	public void finishGame(GameState state) {
		if (currentNode != root)
			currentNode.setChildren(null); // remove falsely created children of last node
		switch (state) {
			case LOST:
				// mark node as lost and set penalty
				// if all sibling are lost, mark parent lost as well
				currentNode.markLostAndDecreaseConfidence(0.5);
				break;
			case WON:
				// todo increase confidence here somehow? Or how should the agent react to
				// replaying won levels?
				currentNode.setWon();
				break;
			default:
				throw new IllegalArgumentException("Illegal GameState " + state);
		}
		setCurrentNode(root);
		root.printHierarchical("");
	}

	/**
	 * Find matching Node even if some pixels aren't matching
	 */
	public void compareScreenshotsReplaceCurrentNode(BufferedImage img) {

		// If there already was a node selected which follows the current node, check
		// that first
		if (currentNode.getLeadsTo() != null) {
			Node leadingTo = currentNode.getLeadsTo();
			if (leadingTo.equalScreenshot(img)) {
				LOG.info("Setting to existing matching sibling...");
				setCurrentNode(leadingTo);
				return;
			}
		}

		// Check if any sibling node matches the screenshot
		LOG.debug("Checking if another node is equal ...");
		for (Node sibling : currentNode.getSiblings()) {
			if (sibling.equalScreenshot(img)) {
				LOG.info("Found matching sibling...");
				currentNode.setLeadsTo(sibling);
				sibling.screenshot = img;
				setCurrentNode(sibling);
				return;
			}
		}

		// if no sibling matches and the node does not have a screenshot yet, it can be
		// set safely
		if (currentNode.screenshot == null) {
			currentNode.screenshot = img;
			return;
		}

		// if screenshots match, do nothing - correct node is already selected.
		if (currentNode.equalScreenshot(img)) {
			return;
		}

		// If they don't match, there might be some inconsistency compared to the
		// previous execution.
		// This might happen when the scaling factor changed or something wrong happened
		// with the execution
		LOG.debug("Didn't find matching sibling. And current node does not match, creating new one ...");
		Node newbie = new Node(currentNode.getPlan());
		newbie.screenshot = img;
		if (currentNode.parent != null) {
			currentNode.parent.addChild(newbie);
		} else {
			currentNode.addChild(newbie);
		}
		setCurrentNode(newbie);
	}

	@Override
	public String toString() {
		return "(DecisionTree root: " + root + ", current: " + currentNode + ")";
	}

	/**
	 * Update the shots of all nodes because of changes in the scaling factor
	 *
	 * @param currentScale the current scale
	 * @param newScale     the new scale
	 */
	public void adaptNodesToNewScalingFactor(double currentScale, double newScale) {
		root.adaptNodesToNewScalingFactor(currentScale, newScale);
	}
}
