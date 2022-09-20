/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab;

import de.uniba.sme.bambirds.common.utils.MathUtil;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;

public class LineSegment {

	public static final int NO_CHANGE = 0;
	public static final int CLOCKWISE = 1;
	public static final int ANTICLOCKWISE = -1;

	private static final double THRESHOLD1 = Math.toRadians(20);
	private static final double JOIN_THRESHOLD = Math.toRadians(5);
	private static final double JOIN_THRESHOLD2 = 40; // 40;

	// angle tracking parameters
	private double prevAngle;
	private double accumChange;
	private int dirChange;

	private final Point start;
	private Point end;
	private Point prevEnd;

	/**
	 * Create a new LineSegment
	 *
	 * @param start of the line segment
	 * @param angle of the current point (in radiants)
	 */
	public LineSegment(final Point start, final double angle) {
		// initialise start and end points
		this.start = start;
		end = start;
		prevEnd = start;

		// tracking parameters
		prevAngle = angle;
		accumChange = 0;
		dirChange = NO_CHANGE;
	}

	/**
	 * Add a new Point to the LineSegment.
	 *
	 * @param p          the point
	 * @param angle      of the following segment
	 * @param threshold2 for the difference between angle and prevAngle
	 * @return 0 if the point was added successfully else the difference between angles
	 */
	public double addPoint(final Point p, final double angle, final double threshold2) {
		if (angle != ConnectedComponent.ANGLE_UNDEFINED) {
			// test point is not a corner
			double diff = angleDiff(angle, prevAngle);

			// if the sign of angle change is reversed
			if (sign(diff) != dirChange) {
				// corner detected
				if (accumChange >= THRESHOLD1) {
					return angleDiff(angle, approximateAngle());
				}

				// reset cumulated results
				if (diff != 0) {
					accumChange = Math.abs(diff);
					dirChange = sign(diff);
				}
			} else {
				accumChange += Math.abs(diff);
				if (accumChange >= threshold2) {
					return angleDiff(angle, approximateAngle());
				}
			}
			prevAngle = angle;
		}
		prevEnd = getEnd();
		end = p;

		return 0;
	}

	/**
	 * draw start point of the line onto canvas.
	 *
	 * @param g    the canvas
	 * @param left the left minimal point
	 * @param top  the top minimal point
	 */
	public void draw(final Graphics2D g, final int left, final int top) {
		g.setColor(Color.CYAN);
		g.fillOval(getStart().x + left - 2, getStart().y + top - 2, 4, 4);
		// g.fillOval(_end.x + left - 1, _end.y + top - 1, 2, 2);
	}

	public double approximateAngle() {
		double xDiff = getStart().x - getEnd().x;
		double yDiff = getStart().y - getEnd().y;

		if (xDiff == 0) {
			return Math.PI / 2;
		} else {
			if (Math.atan(yDiff / xDiff) < 0) {
				return Math.atan(yDiff / xDiff) + Math.PI;
			} else {
				return Math.atan(yDiff / xDiff);
			}
		}
	}

	/**
	 * Joins this line segment with another if the overall angle is similar.
	 *
	 * @param line to join with
	 * @return true if successfully joined, else false
	 */
	public boolean join(final LineSegment line) {
		double angle = approximateAngle();
		double angle2 = line.approximateAngle();
		double diff = Math.abs(angleDiff(angle, angle2));
		double minD = Math.min(distance(getStart(), getEnd()), distance(line.getStart(), line.getEnd()));
		if (diff < JOIN_THRESHOLD || minD * 2 + Math.toDegrees(diff) < JOIN_THRESHOLD2) {
			end = line.getEnd();
			return true;
		}

		return false;
	}

	/**
	 * calculate angle difference a - b <br>
	 * angles in (radians) in range [0, PI).
	 *
	 * @param a first angle
	 * @param b second angle
	 * @return difference a - b (positive for clockwise)
	 */
	public static double angleDiff(final double a, final double b) {
		double diff = a - b;

		if (diff < -MathUtil.PI_2) {
			diff += Math.PI;
		} else if (diff > MathUtil.PI_2) {
			diff -= Math.PI;
		}

		return diff;
	}

	/**
	 * convert the angle a to a representation which is close to the target by
	 * adding/subtracting PI - e.g. closeAngle(179, 0) = -1.
	 *
	 * @param a      the angle
	 * @param target the target
	 * @return a close angle to the target
	 */
	public static double closeAngle(final double a, final double target) {
		return target + angleDiff(a, target);
	}

	/**
	 * return direction of the angle change, clockwise for positive changes.
	 *
	 * @param diff the difference between two angles
	 * @return the direction of the angle change
	 */
	public static int sign(final double diff) {
		if (diff > 0) {
			return CLOCKWISE;
		} else if (diff < 0) {
			return ANTICLOCKWISE;
		} else {
			return NO_CHANGE;
		}
	}

	public static double distance(final Point a, final Point b) {
		int x = a.x - b.x;
		int y = a.y - b.y;
		return Math.sqrt(x * x + y * y);
	}

	public void removeEndPoint() {
		end = prevEnd;
	}

	public Point getStart() {
		return start;
	}

	public Point getEnd() {
		return end;
	}
}
