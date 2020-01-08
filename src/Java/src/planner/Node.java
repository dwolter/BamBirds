package planner;

import ab.demo.other.Shot;
import ab.vision.ABObject;
import helper.CustomLogger;
import meta.ActionRobot;
import shot.SavedShot;

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

public class Node {

	public Target target;
	private ab.demo.other.Shot shot;
	public shot.SavedShot savedShot;

	public int points = 0;
	private double confidence = 0;
	public boolean gameLost = false;

	private int depthLevel = 0;
	public Node parent = null;
	private ArrayList<Node> children = new ArrayList<>();
	public transient BufferedImage screenshot = null;
	public ArrayList<Point> pointList = new ArrayList<>();



	public Node(Target target, SavedShot savedShot) {
		this.savedShot = savedShot;
		this.target = target;
		if (target != null)
			this.confidence = target.getConfidence();
		if (savedShot != null)
			this.shot = savedShot.storedShot;
	}

	public double getConfidence() { return confidence; }

	public int getPoints() { return points; }
	public void setPoints(int points) { this.points = points; }

	public Shot getShot() { return shot; }
	public void setShot(Shot shot) { this.shot = shot; }

	public ArrayList<Node> getChildren() { return new ArrayList<>(this.children); }

	public void setChildren(ArrayList<Node> children) {
		if (children == null)
			this.children = new ArrayList<>();
		else
			this.children = children;
	}

	public void markLostAndDecreaseConfidence(double confidenceMultiplier) {
		this.gameLost = true;
		CustomLogger.info("Current node lost, propagating confidence penalty ...");
		Node n = this;
		int exponent = 1;
		while (n.parent != null) {
			for (Node sibling : n.parent.getChildren()) {
				n.parent.gameLost &= sibling.gameLost;
			}

			double newConfidence = n.getConfidence() - Math.pow(confidenceMultiplier, exponent);
			CustomLogger.info(String.format("New confidence: %.3f for %s", newConfidence, n));
			n.confidence = newConfidence;
			n = n.parent;
			exponent++;
		}
	}

	public ArrayList<Node> getSiblings() {
		if (parent == null)
			return new ArrayList<>();
		return parent.getChildren();
	}

	public void addChild(Node childNode) {
		childNode.parent = this;
		childNode.depthLevel = this.depthLevel + 1;
		children.add(childNode);
	}

	public boolean containsChildWithSaveShot(SavedShot s) {
		for (Node child : children) {
			if (child.equalSavedShot(s))
				return true;
		}
		return false;
	}

	public boolean equalSavedShot(SavedShot shot) {
		return (Math.abs(savedShot.actualAngle - shot.actualAngle) < 0.001);
	}

	/** Compare screenshots for pixel difference */
	public boolean equalScreenshot(BufferedImage img) {
		if (screenshot == null || img == null)
			return false;
		int pxDiff = ActionRobot.get().pixelDifference(screenshot, img);
		return (pxDiff < 10000); // 100x100 px area
	}

	public void printHierarchical(String prefix) {
		CustomLogger.info(prefix + this);
		for (Node child : children)
			child.printHierarchical(prefix + "  ");
	}

	@Override public String toString() {
		double ang = Double.NaN;
		int childCount = 0;

		if (savedShot != null) ang = savedShot.actualAngle;
		if (children != null)  childCount = children.size();

		return String.format("(Node @%.5f, confidence: %.3f, lost: %b, children: %2d, depth: %d)",
				ang, confidence, gameLost, childCount, depthLevel);
	}

	public void savePoints(List<ABObject> abObjects){
		for (ABObject object : abObjects){
			pointList.add(object.getCenter());
		}
	}

	public void compareCurrentSituationWithPrevSituation() {
		if (this.parent == null || this.parent.pointList == null){
			return;
		}
		boolean stillTheSame = true;
		int min = Math.min(this.pointList.size(), this.parent.pointList.size());
		for (int i = 0; i < min; i++){
			if (pointList.get(i).distance(this.parent.pointList.get(i)) > 5){
				stillTheSame = false;
			}
		}
		if (stillTheSame){
			CustomLogger.info("situation still looks the same ----------------");

			if (!containsChildWithSaveShot(this.savedShot)){
				Node newNode = new Node(this.target, this.savedShot);
				newNode.gameLost = true;
				this.addChild(newNode);
				CustomLogger.info("created lost copy of this shot");
			}
		}
	}
}