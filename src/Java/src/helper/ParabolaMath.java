package helper;

import Jama.Matrix;

import java.awt.geom.Point2D;
import java.util.List;

public class ParabolaMath {
	final static private double _g = 1.0;
	final static private double _timeUnit = 815;

	/** Simple call to calculate f(x) = ax^2 + bx + c */
	static private double f(final double[] w, double x) { return w[0] * x * x + w[1] * x + w[2]; }

	/** Tangent or impact angle at any given point */
	static public double tangent(double[] w, double x) { return Math.atan(2 * w[0] * x + w[1]); }

	/** Find location with given impact angle, used in {@link tester.ParabolaTester} */
	static public double tangentToX(double[] w, double tangent) { return (Math.tan(tangent) - w[1]) / (2 * w[0]); }

	/** Calculate absolut error between predicted parabola and target point */
	static public double errorToTargetPoint(final double[] weights, final Point2D.Double target) {
		return Math.abs(target.y - f(weights, target.x));
	}

	// ############################################################################
	// ####
	// ####        Converting to and from velocity
	// ####
	// ############################################################################

	/**
	 * Remove display scaling factor from launch velocity
	 * @return Un-normalized on-screen pixel velocity
	 */
	static public double rescaleVelocity(double velocity, double oldTheta, double oldScale) {
		return parabolaToVelocity(velocityToParabola(oldTheta, velocity, oldScale), 1.0);
	}

	/**
	 * Split velocity vector into {@code dx} and {@code dy} components
	 * @return {@code double[2]} velocity compentens with {@code [vx, vy]}
	 */
	static public double[] velocityToVelocityComponents(double theta, double v) {
		return new double[]{ v * Math.cos(theta), v * Math.sin(theta) };
	}

	/**
	 * Combine velocity components {@code dx} and {@code dy} into single velocity vector
	 * @param v {@code double[2]} velocity compentens with {@code [vx, vy]}
	 */
	static public double velocityComponentsToVelocity(double[] v) {
		double theta = Math.atan2(v[1], v[0]);
		return (v[0] * _g) / Math.cos(theta);
	}

	/**
	 * Combine velocity components {@code dx} and {@code dy} into single velocity vector
	 * @param v {@code double[2]} velocity compentens with {@code [vx, vy]}
	 */
	static public double velocityComponentsToAngle(double[] v) {
		return Math.atan2(v[1], v[0]);
	}

	/**
	 * Calculates parabola weights from velocity components {@code vx} and {@code vy}
	 * @return {@code double[3]} parabola weights with {@code [a, b, 0]}
	 */
	static public double[] velocityComponentsToParabola(double[] v, double scale) {
		return new double[]{ (-0.5 * _g) / (v[0] * v[0] * scale), (v[1] / v[0]), 0 };
	}

	/**
	 *
	 * @param weights Parabola weigths {@code double[2]}
	 * @return {@code double[2]} velocity compentens with {@code [vx, vy]}
	 */
	static public double[] parabolaToVelocityComponents(final double[] weights, double scale) {
		if (weights[0] > 0 && weights[0] < 0.001)
			weights[0] *= -1;
		double ux = Math.sqrt(-0.5 / (weights[0] * _g * scale));
		double uy = weights[1] * ux;
		return new double[]{ ux, uy };
	}

	/**
	 * Calculates parabola weights from velocity and angle
	 * @return {@code double[3]} parabola weights with {@code [a, b, 0]}
	 */
	static public double[] velocityToParabola(double theta, double velocity, double scale) {
		return velocityComponentsToParabola(velocityToVelocityComponents(theta, velocity), scale);
	}

	/**
	 * Velocity at parabola origin
	 * @param weights Parabola weigths {@code double[2]}
	 * @return Normalized velocity
	 */
	static public double parabolaToVelocity(final double[] weights, double scale) {
		return velocityComponentsToVelocity(parabolaToVelocityComponents(weights, scale));
	}

	// ############################################################################
	// ####
	// ####        Tap Time computation
	// ####
	// ############################################################################

	/**
	 * Calculate flight time
	 * @param target Relative to parabola origin
	 * @return Elapsed time upon hit of target point
	 */
	static public int pointToTime(double theta, double velocity, final Point2D.Double target) {
		double[] v = velocityToVelocityComponents(theta, velocity);
		double distance = target.x;// + Math.cos(theta) * 0.4; // pullback STRETCH
		return (int) (distance / v[0] * _timeUnit);
//		double root = Math.sqrt(v[1] * v[1] + 2 * _g * -target.y);
//		double t1 = (-v[1] + root) / -_g;
//		double t2 = (-v[1] - root) / -_g;
//		if (Math.abs(target.x - (t2 * v[0])) < Math.abs(target.x - (t1 * v[0])))
//			t1 = t2;
//		return (int)Math.abs(t1 * _timeUnit);
	}

	/**
	 * Calculate exact position where tap will happen on screen
	 * @param weights Parabola weigths {@code double[3]}
	 * @param elapsedTime Flight time / Tap time
	 * @param xVelocity Velocity X component
	 */
	static public Point2D.Double timeToPoint(final double[] weights, long elapsedTime, double xVelocity) {
		if (elapsedTime <= 0)
			return null;
		double dx = (elapsedTime * xVelocity) / _timeUnit;
		return new Point2D.Double(dx, f(weights, dx));
	}

	// ############################################################################
	// ####
	// ####        Target hit angle / Intersection of two parabolas
	// ####
	// ############################################################################

	/**
	 * Use a different X coordinate as center point and recalculate parabola parameter
	 * @param weights Parabola weigths {@code double[3]}
	 * @param atX Re-Center at this X coordinate
	 * @return Updated weights {@code double[3] = [a, b, 0]} with a same as input
	 */
	static public double[] parabolaWithNewOrigin(final double[] weights, double atX) {
		double b = weights[1] + weights[0] * 2 * atX;
		//double c = weights[2] + weights[1] * atX + weights[0] * atX * atX - f(weights, atX); // will be 0 anyway
		return new double[] { weights[0], b, 0 };
	}

	/**
	 * Invoke projectile equation. Always returns two trajectories.
	 * Can be called multiple times to increase accuracy of the angle.
	 * @param target Relative to parabola origin
	 * @return {@code double[2]} High- and low-shot trajectory angles
	 */
	static public double[] targetToAngles(double velocity, final Point2D.Double target) {
		double v2 = velocity * velocity;
		double gx = _g * target.x;
		double root = Math.sqrt(v2 * v2 - _g * (gx * target.x + 2 * target.y * v2));
		double tangent1 = (v2 - root) / gx;
		double tangent2 = (v2 + root) / gx;
		double theta1 = Math.atan(tangent1);
		double theta2 = Math.atan(tangent2);
		return new double[]{ theta1, theta2 };
	}

	/**
	 * Calculates the intersection between two meeting parabolas. (Warning: No validity checks)
	 * @param first {@code double[3]} Parabola weigths
	 * @param second {@code double[3]} Parabola weigths
	 * @param expected Expected intersection is used to choose nearest point
	 * @return Intersection point
	 */
	static public Point2D.Double intersection(double[] first, double[] second, final Point2D.Double expected) {
		double a = first[0] - second[0];
		if (Math.abs(a) < 1e-8) // avoid very large numbers and division by zero
			return expected;
		double b = (first[1] - second[1]) / a / 2.0;
		double c = Math.sqrt(((second[2] - first[2]) / a) + Math.pow(b, 2));
		double[] solution = new double[]{c - b, -(c + b)};
		if (Math.abs(solution[0] - expected.x) > Math.abs(solution[1] - expected.x)) // match nearest
			solution[0] = solution[1];
		return new Point2D.Double(solution[0], f(first, solution[0]));
	}

	// ############################################################################
	// ####
	// ####        Solve equation, From point cloud to parabola
	// ####
	// ############################################################################

	/**
	 * Fit a solveQuadratic to the given points
	 * @param list On screen points where the trajectory passed through
	 * @return {@code double[3] = [a, b, 0]} with: ax^2 + bx + 0
	 */
	static public double[] solveQuadratic(final List<Point2D.Double> list, final Point2D.Double origin, double scale) {
		double Sx2 = 0.0;
		double Sx3 = 0.0;
		double Sx4 = 0.0;
		double Syx = 0.0;
		double Syx2 = 0.0;

		for (Point2D.Double p : list) {
			// normalise the points
			double x =(p.x - origin.x) / scale;
			double y = (p.y - origin.y) / scale;

			final double x2 = x * x;
			Sx2 += x2;
			Sx3 += x * x2;
			Sx4 += x2 * x2;
			Syx += y * x;
			Syx2 += y * x2;
		}
		final double a = (Syx2 * Sx2 - Syx * Sx3) / (Sx4 * Sx2 - Sx3 * Sx3);
		final double b = (Syx - a * Sx3) / Sx2;
		return new double[]{ -a, b, 0 }; // ax^2 + bx + 0
	}

	/**
	 * Fit parabola using maximum likelihood
	 * @param list On screen points where the trajectory passed through
	 * @return {@code double[3] = [a, b, c]} with: y = ax^2 + bx + c
	 */
	static public double[] solveMaximumLikelihood(List<Point2D.Double> list) {
		int trainingSize = 60;
		double arrayPhiX[][] = new double[trainingSize][3]; // Training set
		double arrayY[][] = new double[trainingSize][1];

		Matrix PhiX, Y;
		Matrix W = new Matrix(new double[] { 0, 0, 0 }, 3);
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

		PhiX = new Matrix(arrayPhiX);
		Y = new Matrix(arrayY);

		// Maximum likelihood
		try {
			W = PhiX.transpose().times(PhiX).inverse().times(PhiX.transpose()).times(Y);
		} catch (Exception e) {
			// if Matrix is singular
			// do nothing
		}
		return W.getRowPackedCopy();
	}
}
