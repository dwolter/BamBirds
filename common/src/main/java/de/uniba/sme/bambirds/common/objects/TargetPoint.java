package de.uniba.sme.bambirds.common.objects;

import java.awt.geom.Point2D;
import java.awt.Point;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;

public class TargetPoint {

	private ABObject target;
	private Point2D.Double targetPoint; // between +90 and -90

	public TargetPoint(ABObject target, Point2D.Double targetPoint) {
		this.target = target;
		this.targetPoint = targetPoint;
	}

	public TargetPoint(ABObject target, Point targetPoint) {
		this.target = target;
		this.targetPoint = new Point2D.Double(targetPoint.getX(),targetPoint.getY());
	}


	/**
	 * Gives the target ID
	 * 
	 * @return the ID of the targeted object as a String
	 */
	public String getTargetID() {
		return target.globalID;
	}

	/**
	 * Gives the target 
	 * 
	 * @return the targeted ABObject
	 */
	public ABObject getTargetObject() {
		return target;
	}

	/**
	 * Gives the target point
	 * 
	 * @return the actual target Point
	 */
	public Point2D.Double getTargetPoint() {
		return targetPoint;
	}

	public String prettyPrint() {
		return String.format("Target: %7s @ Point %s",
				target.globalID, targetPoint.toString());
	}

	public String toString() {
		return String.format("(TargetObject %s:%s)", target.globalID, targetPoint.toString());
	}
}