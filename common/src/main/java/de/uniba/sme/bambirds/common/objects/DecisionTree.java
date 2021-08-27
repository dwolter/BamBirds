package de.uniba.sme.bambirds.common.objects;

import java.awt.image.BufferedImage;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * a class to represent a tree of possible choices for SavedShots
 */
public class DecisionTree {
	private static final Logger log = LogManager.getLogger(DecisionTree.class);
	private Node root;
	private Node currentNode;

	public DecisionTree() {
		root = new Node(null);
		setCurrentNode(root);
	}

	public void resetToRootNode() {
		if (this.currentNode != this.root) {
			this.currentNode.gameLost = true;
			this.currentNode = this.root;
		}
	}

	public Node getCurrentNode() {
		return currentNode;
	}

	public void setCurrentNode(Node currentNode) {
		this.currentNode = currentNode;
	}

	public void finishGame(GameState state) {
		switch (state) {
			case LOST:
				if (currentNode != root)
					currentNode.setChildren(null); // remove falsely created children of last node
				// mark node as lost and set penalty
				// if all sibling are lost, mark parent lost as well
				currentNode.markLostAndDecreaseConfidence(0.5);
				setCurrentNode(root);
				break;
			case WON:
				// todo increase confidence here somehow? Or how should the agent react to
				// replaying won levels?
				setCurrentNode(root);
				break;
			default:
				throw new IllegalArgumentException("Illegal GameState "+state.toString());
		}
		root.printHierarchical("");
	}

	/**
	 * Find matching Node even if some pixel aren't matching
	 */
	public void compareScreenshotsReplaceCurrentNode(BufferedImage img) {
		// TODO: have to check if this has to be moved below the siblings check
		if (currentNode.screenshot == null) {
			currentNode.screenshot = img;
			return;
		}

		// if screenshots match, do nothing - correct node is already selected.
		if (currentNode.equalScreenshot(img))
			return;

		// if they don't match, check one of the other nodes' screenshots matches.
		log.debug("Screenshots don't match, comparing other childNodes ...");
		for (Node sibling : currentNode.getSiblings()) {
			if (sibling.equalScreenshot(img)) {
				log.info("Found matching sibling...");
				sibling.screenshot = img;
				setCurrentNode(sibling);
				return;
			}
		}

		log.debug("Didn't find matching sibling. Creating new one ...");
		Node newbie = new Node(currentNode.plan);
		newbie.screenshot = img;
		if(currentNode.parent != null) {
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
	 * @param currentScale the current scale
	 * @param newScale the new scale
	 */
    public void adaptNodesToNewScalingFactor(double currentScale, double newScale) {
		root.adaptNodesToNewScalingFactor(currentScale, newScale);
    }
}
