package de.uniba.sme.bambirds.common.utils;

import Jama.Matrix;

import java.awt.geom.Point2D;
import java.util.List;

public final class ParabolaMath {
	private static final double GRAVITY = 1.0;
	private static final double TIME_UNIT = 815;

	private ParabolaMath() {
	}

	/**
	 * Simple call to calculate f(x) = ax^2 + bx + c.
	 *
	 * @param w an array of length 3 as parameters a, b and c
	 * @param x the variable x
	 * @return the result of the quadratic formula
	 */
	private static double f(final double[] w, final double x) {
		return w[0] * x * x + w[1] * x + w[2];
	}

	/**
	 * Tangent or impact angle at any given point.
	 */
	public static double tangent(final double[] w, final double x) {
		return Math.atan(2 * w[0] * x + w[1]);
	}

	/**
	 * Find location with given impact angle.
	 */
	public static double tangentToX(final double[] w, final double tangent) {
		return (Math.tan(tangent) - w[1]) / (2 * w[0]);
	}

	/**
	 * Calculate absolut error between predicted parabola and target point
	 */
	public static double errorToTargetPoint(final double[] weights, final Point2D.Double target) {
		return Math.abs(target.y - f(weights, target.x));
	}

	// ############################################################################
	// ####
	// ####        Converting to and from velocity
	// ####
	// ############################################################################

	/**
	 * Remove display scaling factor from launch velocity
	 *
	 * @return Un-normalized on-screen pixel velocity
	 */
	public static double rescaleVelocity(final double velocity, final double oldTheta, final double oldScale) {
		return parabolaToVelocity(velocityToParabola(oldTheta, velocity, oldScale), 1.0);
	}

	/**
	 * Split velocity vector into {@code dx} and {@code dy} components
	 *
	 * @return {@code double[2]} velocity components with {@code [vx, vy]}
	 */
	public static double[] velocityToVelocityComponents(final double theta, final double v) {
		return new double[]{v * Math.cos(theta), v * Math.sin(theta)};
	}

	/**
	 * Combine velocity components {@code dx} and {@code dy} into single velocity vector
	 *
	 * @param v {@code double[2]} velocity components with {@code [vx, vy]}
	 */
	public static double velocityComponentsToVelocity(final double[] v) {
		double theta = Math.atan2(v[1], v[0]);
		return (v[0] * GRAVITY) / Math.cos(theta);
	}

	/**
	 * Combine velocity components {@code dx} and {@code dy} into single velocity vector
	 *
	 * @param v {@code double[2]} velocity components with {@code [vx, vy]}
	 */
	public static double velocityComponentsToAngle(final double[] v) {
		return Math.atan2(v[1], v[0]);
	}

	/**
	 * Calculates parabola weights from velocity components {@code vx} and {@code vy}
	 *
	 * @return {@code double[3]} parabola weights with {@code [a, b, 0]}
	 */
	public static double[] velocityComponentsToParabola(final double[] v, final double scale) {
		return new double[]{(-0.5 * GRAVITY) / (v[0] * v[0] * scale), (v[1] / v[0]), 0};
	}

	/**
	 * @param weights Parabola weights {@code double[2]}
	 * @return {@code double[2]} velocity components with {@code [vx, vy]}
	 */
	public static double[] parabolaToVelocityComponents(final double[] weights, final double scale) {
		if (weights[0] > 0 && weights[0] < 0.001)
			weights[0] *= -1;
		double ux = Math.sqrt(-0.5 / (weights[0] * GRAVITY * scale));
		double uy = weights[1] * ux;
		return new double[]{ux, uy};
	}

	/**
	 * Calculates parabola weights from velocity and angle
	 *
	 * @return {@code double[3]} parabola weights with {@code [a, b, 0]}
	 */
	public static double[] velocityToParabola(final double theta, final double velocity, final double scale) {
		return velocityComponentsToParabola(velocityToVelocityComponents(theta, velocity), scale);
	}

	/**
	 * Velocity at parabola origin
	 *
	 * @param weights Parabola weights {@code double[2]}
	 * @return Normalized velocity
	 */
	public static double parabolaToVelocity(final double[] weights, final double scale) {
		return velocityComponentsToVelocity(parabolaToVelocityComponents(weights, scale));
	}

	// ############################################################################
	// ####
	// ####        Tap Time computation
	// ####
	// ############################################################################

	/**
	 * Calculate flight time
	 *
	 * @param target Relative to parabola origin
	 * @return Elapsed time upon hit of target point
	 */
	public static int pointToTime(final double theta, final double velocity, final Point2D.Double target) {
		double[] v = velocityToVelocityComponents(theta, velocity);
		double distance = target.x; // + Math.cos(theta) * 0.4; // pullback STRETCH
		return (int) (distance / v[0] * TIME_UNIT);
//		double root = Math.sqrt(v[1] * v[1] + 2 * _g * -target.y);
//		double t1 = (-v[1] + root) / -_g;
//		double t2 = (-v[1] - root) / -_g;
//		if (Math.abs(target.x - (t2 * v[0])) < Math.abs(target.x - (t1 * v[0])))
//			t1 = t2;
//		return (int)Math.abs(t1 * _timeUnit);
	}

	/**
	 * Calculate exact position where tap will happen on screen
	 *
	 * @param weights     Parabola weights {@code double[3]}
	 * @param elapsedTime Flight time / Tap time
	 * @param xVelocity   Velocity X component
	 */
	public static Point2D.Double timeToPoint(final double[] weights, long elapsedTime, final double xVelocity) {
		if (elapsedTime <= 0) {
			return null;
		}
		double dx = (elapsedTime * xVelocity) / TIME_UNIT;
		return new Point2D.Double(dx, f(weights, dx));
	}

	// ############################################################################
	// ####
	// ####        Target hit angle / Intersection of two parabolas
	// ####
	// ############################################################################

	/**
	 * Use a different X coordinate as center point and recalculate parabola parameter
	 *
	 * @param weights Parabola weights {@code double[3]}
	 * @param atX     Re-Center at this X coordinate
	 * @return Updated weights {@code double[3] = [a, b, 0]} with a same as input
	 */
	public static double[] parabolaWithNewOrigin(final double[] weights, final double atX) {
		double b = weights[1] + weights[0] * 2 * atX;
		//double c = weights[2] + weights[1] * atX + weights[0] * atX * atX - f(weights, atX); // will be 0 anyway
		return new double[]{weights[0], b, 0};
	}

	/**
	 * Invoke projectile equation. Always returns two trajectories.
	 * Can be called multiple times to increase accuracy of the angle.
	 *
	 * @param target Relative to parabola origin
	 * @return {@code double[2]} High- and low-shot trajectory angles
	 */
	public static double[] targetToAngles(final double velocity, final Point2D.Double target) {
		double v2 = velocity * velocity;
		double gx = GRAVITY * target.x;
		double root = Math.sqrt(v2 * v2 - GRAVITY * (gx * target.x + 2 * target.y * v2));
		double tangent1 = (v2 - root) / gx;
		double tangent2 = (v2 + root) / gx;
		double theta1 = Math.atan(tangent1);
		double theta2 = Math.atan(tangent2);
		return new double[]{theta1, theta2};
	}

	/**
	 * Calculates the intersection between two meeting parabolas. (Warning: No validity checks)
	 *
	 * @param first    {@code double[3]} Parabola weights
	 * @param second   {@code double[3]} Parabola weights
	 * @param expected Expected intersection is used to choose the nearest point
	 * @return Intersection point
	 */
	public static Point2D.Double intersection(final double[] first, final double[] second, final Point2D.Double expected) {
		double a = first[0] - second[0];
		if (Math.abs(a) < 1e-8) // avoid very large numbers and division by zero
			return expected;
		double b = (first[1] - second[1]) / a / 2.0;
		double c = Math.sqrt(((second[2] - first[2]) / a) + Math.pow(b, 2));
		double[] solution = new double[]{c - b, -(c + b)};
		if (Math.abs(solution[0] - expected.x) > Math.abs(solution[1] - expected.x)) {
			// match nearest
			solution[0] = solution[1];
		}
		return new Point2D.Double(solution[0], f(first, solution[0]));
	}

	// ############################################################################
	// ####
	// ####        Solve equation, From point cloud to parabola
	// ####
	// ############################################################################

	/**
	 * Fit a solveQuadratic to the given points
	 *
	 * @param list On screen points where the trajectory passed through
	 * @return {@code double[3] = [a, b, 0]} with: ax^2 + bx + 0
	 */
	public static double[] solveQuadratic(final List<Point2D.Double> list, final Point2D.Double origin, final double scale) {
		double sx2 = 0.0;
		double sx3 = 0.0;
		double sx4 = 0.0;
		double syx = 0.0;
		double syx2 = 0.0;

		for (Point2D.Double p : list) {
			// normalise the points
			double x = (p.x - origin.x) / scale;
			double y = (p.y - origin.y) / scale;

			final double x2 = x * x;
			sx2 += x2;
			sx3 += x * x2;
			sx4 += x2 * x2;
			syx += y * x;
			syx2 += y * x2;
		}
		final double a = (syx2 * sx2 - syx * sx3) / (sx4 * sx2 - sx3 * sx3);
		final double b = (syx - a * sx3) / sx2;
		return new double[]{-a, b, 0}; // ax^2 + bx + 0
	}

	/**
	 * Fit parabola using maximum likelihood
	 *
	 * @param list On screen points where the trajectory passed through
	 * @return {@code double[3] = [a, b, c]} with: y = ax^2 + bx + c
	 */
	public static double[] solveMaximumLikelihood(final List<Point2D.Double> list) {
		int trainingSize = 60;
		double[][] arrayPhiX = new double[trainingSize][3]; // Training set
		double[][] arrayY = new double[trainingSize][1];

		Matrix phiX;
		Matrix y;
		Matrix w = new Matrix(new double[]{0, 0, 0}, 3);
		int i = 0;
		for (Point2D.Double p : list) {
			if (i < trainingSize) {
				arrayPhiX[i][0] = Math.pow(p.x, 2);
				arrayPhiX[i][1] = p.x;
				arrayPhiX[i][2] = 1;
				arrayY[i][0] = p.y;
				i++;
			}
		}

		phiX = new Matrix(arrayPhiX);
		y = new Matrix(arrayY);

		// Maximum likelihood
		try {
			w = phiX.transpose().times(phiX).inverse().times(phiX.transpose()).times(y);
		} catch (Exception e) {
			// if Matrix is singular
			// do nothing
		}
		return w.getRowPackedCopy();
	}
}
