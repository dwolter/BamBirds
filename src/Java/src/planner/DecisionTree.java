package planner;

import ab.vision.GameStateExtractor;
import features.Scene;
import helper.CustomLogger;
import shot.SavedShot;
import ab.demo.other.Shot;

import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Scanner;
import java.util.NoSuchElementException;

import static helper.Constants.DEBUG_ENABLED;

/**
 * a class to represent a tree of possible choices for SavedShots
 */
public class DecisionTree {
	private Node root;
	private Node currentNode;

	public DecisionTree(){
		root = new Node(null, null);
		setCurrentNode(root);
	}

	public void resetToRootNode(){
		if (this.currentNode != this.root){
			this.currentNode.gameLost = true;
			this.currentNode = this.root;
		}
	}

	public Node getCurrentNode() { return currentNode; }
	public void setCurrentNode(Node currentNode) { this.currentNode = currentNode; }

	public void finishGame(GameStateExtractor.GameState state) {
		switch (state) {
		case LOST:
			if (currentNode != root)
				currentNode.setChildren(null); // remove falsely created children of last node
			// mark node as lost and set penalty
			// if all sibling are lost, mark parent lost as well
			currentNode.markLostAndDecreaseConfidence(0.5);
			setCurrentNode(root);
		case WON:
			// todo increase confidence here somehow? Or how should the agent react to replaying won levels?
			setCurrentNode(root);
		}
		if (DEBUG_ENABLED)
			root.printHierarchical("");
	}

	/**
	 * Initialize child nodes list with available targets. Does not create duplicates
	 * @param targetList Target list from Prolog {@link PrologPlanner#planSynchronously(long)}
	 * @param shotsList SavedShot list from {@link Scene#savedShots }
	 */
	public void createChildNodesFromTargetList(List<Target> targetList, List<SavedShot> shotsList) {
		for (Target target : targetList) {
			int impactAngle = target.getAngle();
			String chosenTargetId = target.getTargetId();
            String strategy = target.getDebugInfo();

            if ((strategy.length() > 0) && (strategy.charAt(0) == '/')) { // handle Prolog generated shots
                try {
                    Scanner sc = new Scanner(strategy).useDelimiter("/");
                    Shot rawShot = new Shot(sc.nextInt(), sc.nextInt(), sc.nextInt(), sc.nextInt(), 0, sc.nextInt());
                    double[] dummyParabola = new double[]{ 0.5, 0.5 };
                    SavedShot s = new SavedShot(rawShot, chosenTargetId, impactAngle, impactAngle, dummyParabola, true);
                    System.out.println("[DecTree] parsed shot:" + rawShot.toString());
                    if (!currentNode.containsChildWithSaveShot(s)) {
                        currentNode.addChild(new Node(target, s));
                    }
                } catch (NoSuchElementException e) {
                    System.err.println("[DecTree] failed to parse special shot: " + e.getMessage());
                }

            } else {
                for (SavedShot s : shotsList) { // FIXME: use efficient search!!
                    if (s.impactAngle == impactAngle && chosenTargetId.equalsIgnoreCase(s.targetID)) {
                        // check if target is already in one of the childNodes
                        if (!currentNode.containsChildWithSaveShot(s)) {
                            currentNode.addChild(new Node(target, s));
                        }
                        break;
                    }
                }
            }
		}
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
		CustomLogger.info("[DecisionTree] Screenshots don't match, comparing other childNodes ...");
		for (Node sibling : currentNode.getSiblings()) {
			if (sibling.equalScreenshot(img)) {
				CustomLogger.info("[DecisionTree] Found matching sibling...");
				sibling.screenshot = img;
				setCurrentNode(sibling);
				return;
			}
		}

		CustomLogger.info("[DecisionTree] Didn't find matching sibling. Creating new one ...");
		Node newbie = new Node(currentNode.target, currentNode.savedShot);
		newbie.screenshot = img;
		currentNode.parent.addChild(newbie);
		setCurrentNode(newbie);
	}

	@Override public String toString() {
		return "(DecisionTree root: " + root + ", current: " + currentNode + ")";
	}
}
