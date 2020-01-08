package shot;

import ab.demo.other.Shot;
import ab.vision.ABObject;
import ab.vision.ABType;
import ab.vision.real.shape.Poly;
import ab.vision.real.shape.Rect;
import database.Slingshot;
import helper.CustomLogger;
import meta.ActionRobot;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class ShotPlanner {
	private Slingshot _slinghot;
	private double _scalingFactor = 1.005;
	private ABType _birdType;

	public ShotPlanner(Slingshot sling, double scalingFactor, ABType birdType) {
		_slinghot = sling;
		_birdType = birdType;
		updateInternalScalingFactor(scalingFactor);
		refresh();
	}

	/**
	 * A bit stupid to have variables in ShotHelper, but we need to update them.
	 * In case someone else changed these, since the whole class is static.
	 */
	private void refresh() { ShotHelper.setProperties(_scalingFactor, _birdType); }

	/** After a shot get the display parabola and adjust scaling appropriately */
	public double calculateScalingFactorAndUpdate(final BufferedImage img, Point releasePoint, long tapTime) {
		refresh();
		VisionTraj vision = new VisionTraj(img);
		// never ever write here to img, it is used in Meta later

		// Estimate tap point and get list of trajectory points
		double theta = ShotHelper.releasePointToAngle(releasePoint);
		Point2D.Double estTapPoint = ShotHelper.predictLocationAfterTime(theta, _slinghot, tapTime);

		List<Point2D.Double> pts = new LinkedList<>();
		Point2D.Double actualTap = vision.findTrajectory(_slinghot, estTapPoint, pts);
		pts = vision.filteredTrajectory(pts, actualTap, _slinghot.pivot, theta, _slinghot.bounds.width, false, true);
		if (pts == null || pts.size() < 6)
			return _scalingFactor;

		ShotParabola para = new ShotParabola(theta, _slinghot.pivot, _slinghot.getSceneScale(), pts);
		if (para.velocity == 0 || Double.isNaN(para.velocity))
			return _scalingFactor;

		para.draw(ActionRobot.get().doScreenShot(), estTapPoint);
		updateInternalScalingFactor(ShotHelper.recalculateScalingFactor(para.velocity, theta));
		CustomLogger.info(String.format("[ShotPlanner] New scaling factor: %1.6f", _scalingFactor));
		return _scalingFactor;
	}

	/** Update internal scaling factor as well as {@link ShotHelper#scaleFactor} */
	private void updateInternalScalingFactor(double factor) {
		if (Double.isNaN(factor)) return;
		_scalingFactor = factor;
		refresh();
	}

	/**
	 * Calculate the two high and low shot launch points for target
	 * @return {@code double[2] = [low, high]} Shots actual angle, NaN if not available
	 */
	public double[] predict(Point2D.Double targetPoint) {
		refresh();
		return ShotHelper.estimateLaunchPoint(_slinghot, targetPoint);
	}

	/**
	 * Get tap time based on the bird type on sling
	 * @param theta Actual angle, <b>NOT</b> release point. <br />
	 *              To convert release point use {@link ShotHelper#launchToActual(double)}
	 */
	public int getTapTime(double theta, Point2D.Double targetPoint, ABType birdType) {
		refresh();
		double interval = 0;
		switch (birdType) {
			case BlackBird: interval = 1.05; break;
			case BlueBird: interval = 0.80; break;
			case WhiteBird: interval = 0.90; break;
			case YellowBird: interval = 0.85; break;
			default: break;
		}
		return (int)(ShotHelper.predictTime(theta, _slinghot, targetPoint) * interval);
	}

	/**
	 * Calculate and return the impact angle at target (in degrees)
	 * @param theta Actual angle, <b>NOT</b> release point. <br />
	 *              To convert release point use {@link ShotHelper#launchToActual(double)}
	 */
	public int getImpactAngle(double theta, Point2D.Double targetPoint) {
		refresh();
		double rad = ShotHelper.predictImpactAngle(theta, _slinghot, targetPoint);
		return (int)Math.toDegrees(rad);
	}

	/**
	 * @param theta Actual angle, <b>NOT</b> release point. <br />
	 *              To convert release point use {@link ShotHelper#launchToActual(double)}
	 * @return {@code double[3]} parabola weights with {@code [a, b, 0]}
	 */
	public double[] parabolaForActualAngle(double theta) {
		refresh();
		return ShotHelper.angleToParabola(theta, _slinghot.getSceneScale());
	}

	/** Clip complex polygons at target point. Otherwise it will falsely flag non reachable after a hit */
	private Polygon clippedPolygon(final Polygon poly, int clipRight) {
		Polygon polyNew = new Polygon();
		for (int i = 0; i < poly.npoints; i++) {
			int ii = (i+1) % poly.npoints;
			if (poly.xpoints[i] > clipRight && poly.xpoints[ii] > clipRight) {
				// both outside
				continue;
			} else if (poly.xpoints[i] <= clipRight && poly.xpoints[ii] <= clipRight) {
				// both inside
				polyNew.addPoint(poly.xpoints[ii], poly.ypoints[ii]);
			} else if (poly.xpoints[i] <= clipRight && poly.xpoints[ii] > clipRight) {
				// going out
				float percent = (clipRight - poly.xpoints[ii]) / (float)(poly.xpoints[i] - poly.xpoints[ii]);
				polyNew.addPoint(clipRight, (int)(poly.ypoints[ii] + percent * (poly.ypoints[i]-poly.ypoints[ii])));
			} else {
				// going in
				float percent = (clipRight - poly.xpoints[i]) / (float)(poly.xpoints[ii] - poly.xpoints[i]);
				polyNew.addPoint(clipRight, (int)(poly.ypoints[i] + percent * (poly.ypoints[ii] - poly.ypoints[i])));
				polyNew.addPoint(poly.xpoints[ii], poly.ypoints[ii]);
			}
		}
		return polyNew;
	}

	/** Intersection of parabola with complex polygons */
	private boolean intersectsPoly(Polygon poly, double[] fn, int clipRight, double padding) {
		for (int i = 0; i < poly.npoints; i++) {
			if (poly.xpoints[i] > clipRight) {
				poly = clippedPolygon(poly, clipRight);
				break;
			}
		}
		boolean prevA = false;
		boolean prevB = false;
		for (int i = 0; i < poly.npoints; i++) {
			double x = poly.xpoints[i] - _slinghot.pivot.x;
			double y = (_slinghot.pivot.y - (fn[0] * x * x + fn[1] * x));
			boolean above = (poly.ypoints[i] < (y - padding));
			boolean below = (poly.ypoints[i] > (y + padding));
			if (i > 0 && (prevA != above || prevB != below))
				return true;
			prevA = above;
			prevB = below;
		}
		return false;
	}

	/** Simple bounding box intersection with parabola function */
	private boolean intersectsBoundingBox(Rectangle box, double[] fn, int clipRight, double padding) {
		double x1 = box.x - _slinghot.pivot.x - padding;
		double x2 = Math.min(clipRight, box.x + box.width + padding) - _slinghot.pivot.x;
		double y1 = (_slinghot.pivot.y - (fn[0] * x1 * x1 + fn[1] * x1));
		double y2 = (_slinghot.pivot.y - (fn[0] * x2 * x2 + fn[1] * x2));
		if ((y1 + padding) < box.y && (y2 + padding) < box.y) // above the block
			return false;
		if ((y1 - padding) > box.y + box.height && (y2 - padding) > box.y + box.height) // below the block
			return false;
		return true;
	}

	/**
	 * Follow the trajectory points and checks for collision with other blocks
	 * @param sortedCandidates List containing all scene {@code ABObject} sorted by X coordinate (no birds)
	 */
	private boolean isReachable(double[] parabola, ABObject target, List<ABObject> sortedCandidates, double padding) {
		refresh();
		int targetX = (int)target.getCenterX();
		for (ABObject block : sortedCandidates) {
			if (block.x > targetX) // block is right of current trajectory point
				break;
			if (block.equals(target))
				continue; // maybe a smaller block is on top, keep going

			if (intersectsBoundingBox(block.getBounds(), parabola, targetX, padding)) {
				switch (block.shape) {
					case Rect:
						if (!(block instanceof Rect) || intersectsPoly(((Rect)block).p, parabola, targetX, padding))
							return false;
						break;
					case Poly:
						if (!(block instanceof Poly))
							return false;
						Polygon poly = ((Poly)block).polygon;
						//if (intersectsPoly(poly, w, targetX)) return false;
						double startX = (block.x - _slinghot.pivot.x);
						double endX = Math.min(targetX, block.x + block.width) - _slinghot.pivot.x;
						for (double x = startX; x < endX; x+=3) {
							double y = (_slinghot.pivot.y - (parabola[0] * x * x + parabola[1] * x));
							if (poly.contains(x + _slinghot.pivot.x, y)
									|| poly.contains(x + _slinghot.pivot.x, y + padding)
									|| poly.contains(x + _slinghot.pivot.x, y - padding))
								return false;
						}
						break;
					case Circle: // TODO: circle estimation? not high priority but could add reachable targets
					case Triangle: return false; // since its already inside the bounding box
				}
			}
		}
		return true;
	}

	/**
	 * Return a list of {@link SavedShot}s for given target point
	 * @param targetPoint Can be different than the {@code targetObject} center point
	 * @param targetObject Target object is used for collision detection and {@code globalID}
	 * @param allBlocks Sorted list  of {@code ABObject} without birds (sorted by X coordinate)
	 * @return List containing all shots no matter if reachable
	 */
	public List<SavedShot> savedShotsForTarget(Point2D.Double targetPoint, ABObject targetObject, List<ABObject> allBlocks, double birdSize) {
		refresh();
		Point pivot = _slinghot.getPivotPoint();
		double[] releaseAngle = predict(targetPoint);
		List<SavedShot> shotList = new ArrayList<>();
		for (int i = 0; i < 2; i++) { // two points, high and low shot
			double theta = releaseAngle[i];
			if (Double.isNaN(theta))
				continue;
			double[] parabola = parabolaForActualAngle(theta);
			boolean reachable = isReachable(parabola, targetObject, allBlocks, birdSize);
			int impactAngle = getImpactAngle(theta, targetPoint);
			int tapTime = getTapTime(theta, targetPoint, _birdType);
			Point releasePoint = ShotHelper.angleToReleasePoint(theta);
			Shot s = new Shot(pivot.x, pivot.y, releasePoint.x, releasePoint.y, 0, tapTime);
			shotList.add(new SavedShot(s, targetObject.globalID, theta, impactAngle, parabola, reachable));
            //System.out.println("Shot@" + targetPoint.x + "," + targetPoint.y + "@" + theta + "-->" + releasePoint.x + "," + releasePoint.y);
		}
		return shotList;
	}
}
