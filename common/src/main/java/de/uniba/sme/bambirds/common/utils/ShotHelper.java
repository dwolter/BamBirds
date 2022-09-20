package de.uniba.sme.bambirds.common.utils;

import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;

import java.awt.Point;
import java.awt.geom.Point2D;
import java.util.LinkedList;
import java.util.List;

public final class ShotHelper {
	private static double scaleFactor = 1.005;
	private static double highAngleBegin;
	private static double[] highAngleVelocity;
	private static double[] highAngleChange;

	static {
		setProperties(scaleFactor, ABType.RedBird);
	}

	private ShotHelper() {
	}

	public static void setProperties(final double scaling, final ABType birdType) {
		scaleFactor = scaling;
		//CustomLogger.info("[ShotHelper] Set bird type: " + birdType);
		highAngleBegin = Settings.highAngleBegin[birdType.id()];
		highAngleChange = Settings.highAngleChange[birdType.id()];
		highAngleVelocity = Settings.highAngleVelocity[birdType.id()];
	}

	private static double fn(final double[] w, final double x) {
		return w[0] * x * x + w[1] * x + w[2];
	}

	public static double fYellowVelocity(final double x) {
		return fn(Settings.yellowBirdVelocity, x);
	}

	/**
	 * Quadratic function over the velocity for each angle.
	 */
	private static double fLaunchVelocity(final double x) {
		if (isVeryHighAngle(x)) {
			return fn(highAngleVelocity, x);
		}
		return fn(Settings.lowAngleVelocity, x);
	}

	/**
	 * Quadratic function to convert actual angle to launch angle.
	 */
	private static double fChangeAngle(final double x) {
		if (isVeryHighAngle(x)) {
			return fn(highAngleChange, x);
		}
		return fn(Settings.lowAngleChange, x);
	}

	/**
	 * Modified velocity used for shot prediction.
	 */
	public static double angleToVelocity(final double radiant) {
		return scaleFactor * fLaunchVelocity(radiant);
	}

	/**
	 * Returns true if angle is over {71 - 78} degrees, depending on the bird.
	 */
	private static boolean isVeryHighAngle(final double theta) {
		return theta >= highAngleBegin;
	}

	/**
	 * Convert internal angle to display launch angle.
	 */
	public static double actualToLaunch(final double theta) {
		return theta + fChangeAngle(theta);
	}

	/**
	 * Convert launch angle to internal angle.
	 */
	public static double launchToActual(final double theta) {
		double upper = Math.toRadians(86);
		double lower = Math.toRadians(-25);
		while ((upper - lower) > 1e-4) {
			double midAngle = lower + (upper - lower) / 2;
			double err1 = Math.abs(theta - actualToLaunch(midAngle + 1e-5));
			double err2 = Math.abs(theta - actualToLaunch(midAngle - 1e-5));
			if (err1 > err2) {
				upper = midAngle;
			} else {
				lower = midAngle;
			}
		}
		return upper;
	}

	// ############################################################################
	// ####
	// ####        Conversion of trajectory attributes
	// ####
	// ############################################################################

	/**
	 * Calculate pixel offset representation to launch angle.
	 *
	 * @return Pixel coordinate deltas (relative position)
	 */
	public static Point angleToReleasePoint(final double theta, final Slingshot s) {
		double mag = 1000;
		if (theta < 0) {
			// SlingY + DragY must be greater than 100 otherwise the shot will be discarded
			mag = s.y - 100;
		}
		double launchTheta = actualToLaunch(theta);
		return new Point((int) (-mag * Math.cos(launchTheta)), (int) (mag * Math.sin(launchTheta)));
	}

	/**
	 * Calculate launch angle from clicked point.
	 *
	 * @param releasePoint Display pixel coordinates (relative position)
	 * @return Actual angle
	 */
	public static double releasePointToAngle(final Point releasePoint) {
		double theta = -Math.atan2(-releasePoint.y, -releasePoint.x);
		return launchToActual(theta);
	}

	/**
	 * Calculate parabola weight from launch angle.
	 *
	 * @param theta Actual angle, <b>NOT</b> release point.
	 *              To convert release point use {@link #launchToActual(double)}
	 * @return {@code double[3]} parabola weights with {@code [a, b, 0]}
	 */
	public static double[] angleToParabola(final double theta, final double scale) {
		double[] v = ParabolaMath.velocityToVelocityComponents(theta, fLaunchVelocity(theta));
		return ParabolaMath.velocityComponentsToParabola(v, scale);
	}

	/**
	 * Small adjustments to scaling factor during repeated plays.
	 *
	 * @param v     Measured velocity after shot
	 * @param theta Launch angle used for shot
	 * @return New scaling factor for this level
	 */
	public static double recalculateScalingFactor(final double v, final double theta) {
		if (theta < Math.toRadians(5) || theta > Math.toRadians(83)) {
			return scaleFactor;
		}

		double temp = v / fLaunchVelocity(theta);
		// avoid setting velocity to NaN
		if (temp != temp || temp > 1.1 || temp < 0.9) {
			return scaleFactor;
		}

		if (theta > Math.toRadians(50) && !isVeryHighAngle(theta)) {
			return temp;
		}
		return temp * 0.6 + scaleFactor * 0.4; // small angles are already filtered out
	}

	/**
	 * Find the tap point for a given angle (yellow bird).
	 *
	 * @param theta  Fixed launch Actual-angle
	 * @param target Target point (absolute coordinates)
	 * @return Tap point (absolute coordinates)
	 */
	public static Point2D.Double predictYellowBirdTapPoint(final double theta, final Slingshot sling, final Point2D.Double target) {
		double tx = target.x - sling.getPivot().x;
		double ty = sling.getPivot().y - target.y;
		double[] f = ShotHelper.angleToParabola(theta, sling.getSceneScale());
		double tapX = tx - Math.sqrt(tx * tx - (ty - f[1] * tx) / f[0]);
		double tapY = 0;

		for (int retry = 20; retry > 0; retry--) {
			tapY = f[0] * tapX * tapX + f[1] * tapX;
			double thetaTap = ParabolaMath.tangent(f, tapX);
			double v = fYellowVelocity(thetaTap);
			double rdx = (tx - tapX) / sling.getSceneScale();
			double rdy = (ty - tapY) / sling.getSceneScale();
			double[] t = ParabolaMath.targetToAngles(v, new Point2D.Double(rdx, rdy));
			double newX1 = ParabolaMath.tangentToX(f, t[0]);
			//double newX2 = ParabolaMath.tangentToX(f, t[1]); // I think, its always t[0]!
			//if (Math.abs(newX1 - tapX) > Math.abs(newX2 - tapX))
			//	newX1 = newX2;
			if (Math.abs(newX1 - tapX) < 1e-5) {
				break;
			}
			tapX = newX1;
		}
		return new Point2D.Double(tapX + sling.getPivot().x, sling.getPivot().y - tapY);
	}

	/**
	 * Calculate flight time. Can be used as tap time t2 in {@link ActionRobotJava#shoot}
	 *
	 * @param theta Actual angle, <b>NOT</b> release point.
	 *              To convert release point use {@link #launchToActual(double)}
	 * @return Elapsed time upon hit of target point
	 */
	public static int predictTime(final double theta, final Slingshot sling, final Point2D.Double target) {
		// takes 100ms to drag bird on sling until shot, (maybe not)
		return 100 + ParabolaMath.pointToTime(theta, angleToVelocity(theta), relativeToSling(target, sling));
	}

	/**
	 * Calculate impact angle for target.
	 *
	 * @param theta Actual angle, <b>NOT</b> release point.
	 *              To convert release point use {@link #launchToActual(double)}
	 */
	public static double predictImpactAngle(final double theta, final Slingshot sling, final Point2D.Double target) {
		double[] w = angleToParabola(theta, 1);
		return ParabolaMath.tangent(w, relativeToSling(target, sling).x);
	}

	/**
	 * Calculate exact position where tap will happen on screen.
	 *
	 * @param theta Actual angle, <b>NOT</b> release point.
	 *              To convert release point use {@link #launchToActual(double)}
	 */
	public static Point2D.Double predictLocationAfterTime(final double theta, final Slingshot sling, long elapsedTime) {
		// takes 100ms to drag bird on sling until shot, (maybe not)
		elapsedTime -= 100;
		if (elapsedTime <= 0) {
			return null;
		}
		double[] v = ParabolaMath.velocityToVelocityComponents(theta, angleToVelocity(theta));
		double[] w = ParabolaMath.velocityComponentsToParabola(v, sling.getSceneScale());
		Point2D.Double p = ParabolaMath.timeToPoint(w, elapsedTime, v[0] * sling.getSceneScale());
		assert p != null;
		return new Point2D.Double(sling.getPivot().x + p.x, sling.getPivot().y - p.y);
	}

	// ############################################################################
	// ####
	// ####        Display related
	// ####
	// ############################################################################

	public static Point2D.Double unnormalizedPoint(final Point2D.Double origin, final Point2D.Double target, final double scale) {
		return new Point2D.Double(origin.x + target.x * scale, origin.y - target.y * scale);
	}

	/**
	 * Returns a point relative to slingshot and divided by screen scaling
	 */
	private static Point2D.Double relativeToSling(final Point2D.Double target, final Slingshot sling) {
		return normalizedPoint(sling.getPivot(), new Point2D.Double(target.x, target.y), sling.getSceneScale());
	}

	/**
	 * Scaled relative point, eg. for Slingshot
	 */
	public static Point2D.Double normalizedPoint(final Point2D.Double origin, final Point2D.Double target, final double scale) {
		return new Point2D.Double((target.x - origin.x) / scale, (origin.y - target.y) / scale);
	}

	/**
	 * Find the two release points for given target.
	 *
	 * @return {@code double[2] = [low, high]} Shots actual angle, NaN if not available
	 */
	public static double[] estimateLaunchPoint(final Slingshot sling, final Point2D.Double target) {
		Point2D.Double rel = relativeToSling(target, sling);
		double[] t = ParabolaMath.targetToAngles(angleToVelocity(Math.toRadians(40)), rel);
		for (int a = 0; a < 2; a++) { // for both shots
			if (isVeryHighAngle(t[a])) {
				t[a] = predictAngleForVeryHighShot(rel);
			} else {
				for (int i = 5; i > 0; i--) { // max 5 refinements
					double newTheta = ParabolaMath.targetToAngles(angleToVelocity(t[a]), rel)[a];
					if (Double.isNaN(newTheta) || Math.abs(newTheta - t[a]) < 1e-5) {
						break;
					}
					t[a] = newTheta;
				}
			}
		}
		return t;
	}

	/**
	 * Internal absolute error with chosen theta for very high angles to target point.
	 */
	private static double err(final double theta, final Point2D.Double relPoint) {
		double[] w = ParabolaMath.velocityToParabola(theta, angleToVelocity(theta), 1);
		return ParabolaMath.errorToTargetPoint(w, relPoint);
	}

	/**
	 * Iterative estimation of correct launch angle for shot > 75°.
	 */
	private static double predictAngleForVeryHighShot(final Point2D.Double relPoint) {
		double upper = Math.toRadians(86);
		double lower = highAngleBegin;
		while ((upper - lower) > 1e-4) {
			double midAngle = lower + (upper - lower) / 2;
			if (err(midAngle + 1e-5, relPoint) > err(midAngle - 1e-5, relPoint)) {
				upper = midAngle;
			} else {
				lower = midAngle;
			}
		}
		return upper;
	}

	/**
	 * Get onscreen parabola regardless of slingshot.
	 *
	 * @param list On screen points where the trajectory passed through
	 * @return {@code double[3] = [a, b, c]} with: ax^2 + bx + c
	 * <p> Returns [0, 0, 0] if estimation not convincing
	 */
	public static double[] parabolaFromPointCloud(final List<Point2D.Double> list) {
		LinkedList<Point2D.Double> l = new LinkedList<>(list);
		double[] w = new double[]{0, 0, 0};
		for (int retry = 5; retry > 0; retry--) {
			if (l.size() < 6) { // too few points for a convincing estimation
				return new double[]{0, 0, 0};
			}

			w = ParabolaMath.solveMaximumLikelihood(l);

			double maxError = 0;
			int maxErrorIndex = 0;
			for (int i = 0; i < l.size(); i++) { // outlier detection
				Point2D.Double p = l.get(i);
				double sqErr = Math.pow(p.y - fn(w, p.x), 2);

				if (sqErr > maxError) {
					maxError = sqErr;
					maxErrorIndex = i;
				}
			}

			// TODO: tweak threshold parameter or remove largest error only
			if (maxError < 25) { // perfect, we found the estimation
				break;
			}

			// else: remove point with the highest error
			//Point2D.Double p = l.get(maxErrorIndex);
			//System.out.println(String.format("removing point (%1.2f, %1.2f) with err: %f", p.x, p.y, maxError));
			l.remove(maxErrorIndex); // can't be -1 since maxError would be 0 too
		}
		return w;
	}

	/**
	 * Update a Shot because of a change in the scaling factor, which would otherwise make it unusable.
	 *
	 * @param slingshot            The Slingshot
	 * @param birdType             The bird of the shot
	 * @param shot                 The shot to adjust
	 * @param currentScalingFactor The current scaling factor
	 * @param newScalingFactor     The new scaling factor calculated using {@link #recalculateScalingFactor(double, double)}
	 */
	public static void updateShotToNewScalingFactor(final Slingshot slingshot, final ABType birdType, final Shot shot, final double currentScalingFactor, final double newScalingFactor) {
		setProperties(currentScalingFactor, birdType);

		Point oldReleasePoint = new Point(shot.getDragX(), shot.getDragY());
		Point2D.Double targetPoint = new Point2D.Double(shot.getTargetX(), shot.getTargetY());

		// Calculate the previous tap ratio
		double oldTheta = releasePointToAngle(oldReleasePoint);
		int tof = predictTime(oldTheta, slingshot, targetPoint);
		double ratioOfTap = (double) shot.getTapTime() / tof;

		int lowOrHigh = oldTheta > Math.toRadians(45) ? 1 : 0;

		// Calculate the new Angle and releasePoint
		setProperties(newScalingFactor, birdType);
		double newTheta = estimateLaunchPoint(slingshot, targetPoint)[lowOrHigh];
		tof = predictTime(newTheta, slingshot, targetPoint);
		int newTapTime = (int) (tof * ratioOfTap);
		Point newReleasePoint = angleToReleasePoint(newTheta, slingshot);

		// Update shot variables
		shot.setDragX(newReleasePoint.x);
		shot.setDragY(newReleasePoint.y);
		shot.setTapTime(newTapTime);
	}
}
