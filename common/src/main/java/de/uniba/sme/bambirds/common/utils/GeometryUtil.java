package de.uniba.sme.bambirds.common.utils;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

public final class GeometryUtil {

	private GeometryUtil() {
	}

	public static List<Point> polygonPoints(final Polygon p) {
		List<Point> points = new ArrayList<>();
		PathIterator pi = p.getPathIterator(null);
		while (!pi.isDone()) {
			double[] coordinates = new double[6];
			int status = pi.currentSegment(coordinates);
			switch (status) {
				case PathIterator.SEG_MOVETO:
				case PathIterator.SEG_LINETO:
					points.add(new Point(MathUtil.round(coordinates[0]), MathUtil.round(coordinates[1])));
				default:
					break;
			}
			pi.next();
		}
		return points;
	}

	public static double perpendicular(final double angle) {
		return angle > Math.PI / 2 ? angle - Math.PI / 2 : angle + Math.PI / 2;
	}

	public static Polygon createPolygon(final double angle, final double centerX, final double centerY, final double pWidth, final double pHeight) {

		double angle2 = perpendicular(angle);

		// starting point for drawing
		double xs;
		double ys = centerY + Math.sin(angle) * pHeight / 2 + Math.sin(Math.abs(Math.PI / 2 - angle)) * pWidth / 2;
		if (angle < Math.PI / 2) {
			xs = centerX + Math.cos(angle) * pHeight / 2 - Math.sin(angle) * pWidth / 2;
		} else if (angle > Math.PI / 2) {
			xs = centerX + Math.cos(angle) * pHeight / 2 + Math.sin(angle) * pWidth / 2;
		} else {
			xs = centerX - pWidth / 2;
		}

		Polygon p = new Polygon();
		p.addPoint(MathUtil.round(xs), MathUtil.round(ys));

		xs -= Math.cos(angle) * pHeight;
		ys -= Math.sin(angle) * pHeight;
		p.addPoint(MathUtil.round(xs), MathUtil.round(ys));

		xs -= Math.cos(angle2) * pWidth;
		ys -= Math.sin(angle2) * pWidth;
		p.addPoint(MathUtil.round(xs), MathUtil.round(ys));

		xs += Math.cos(angle) * pHeight;
		ys += Math.sin(angle) * pHeight;
		p.addPoint(MathUtil.round(xs), MathUtil.round(ys));

		return p;

	}

	/**
	 * Generate a list of normal vectors for a Polygon. The Vectors are saved as
	 * Points with integers for x and y within the range [0,254] <br/>
	 * where 0 = -127, 127 = 0 and 254 = 127
	 *
	 * @param p Polygon to create the Norm vectors for
	 * @return List of Vectors
	 */
	public static List<Point> listOfNormsInt(final Polygon p) {
		List<Point2D> doubleVectors = listOfNorms(p);
		List<Point> intVectors = new ArrayList<>();

		for (Point2D vector : doubleVectors) {
			int vx = (int) Math.round(vector.getX() * 127);
			int vy = (int) Math.round(vector.getY() * 127);
			intVectors.add(new Point(vy + 127, vx + 127));
		}
		return intVectors;
	}

	/**
	 * Generate a list of normal vectors for a Polygon. The Vectors are saved as
	 * Points with integers for x and y within the range [0,254] <br/>
	 * where 0 = -127, 127 = 0 and 254 = 127
	 *
	 * @param p Polygon to create the Norm vectors for
	 * @return List of Vectors
	 */
	public static List<Point2D> listOfNorms(final Polygon p) {
		List<Point2D> vectors = new ArrayList<>();
		boolean isClockwise = clockwise(p);

		for (int i = 0; i < p.npoints; i++) {
			int nextI = (i + 1 == p.npoints ? 0 : i + 1);
			double dx = (p.xpoints[nextI] - p.xpoints[i]);
			double dy = (p.ypoints[nextI] - p.ypoints[i]);
			double len = Math.hypot(dx, dy);
			double vx = dx / len;
			double vy = dy / len;
			if (isClockwise) {
				vx = -vx;
			} else {
				vy = -vy;
			}
			vectors.add(new Point2D.Double(vx, vy));
		}
		return vectors;
	}

	/**
	 * Check if the Polygon Points are in clockwise order.
	 *
	 * @param p Polygon to check
	 * @return True if the Polygon points are in clockwise order, otherwise False
	 */
	public static boolean clockwise(final Polygon p) {
		int sum = 0;
		for (int i = 0; i < p.npoints; i++) {
			int nextI = (i + 1 == p.npoints ? 0 : i + 1);
			sum += (p.xpoints[nextI] - p.xpoints[i]) * (p.ypoints[nextI] + p.ypoints[i]);
		}
		return sum < 0;
	}

	/**
	 * generate a bresenham Line from Point (x0|y0) to Point (x1|y1).
	 *
	 * @param x0 x coordinate of the first point
	 * @param y0 y coordinate of the first point
	 * @param x1 x coordinate of the second point
	 * @param y1 y coordinate of the second point
	 * @return the list of points for bresenham
	 */
	public static List<Point> bresenham(final int x0, final int y0, final int x1, final int y1) {
		return bresenham(new Point(x0, y0), new Point(x1, y1));
	}

	/**
	 * generate a bresenham Line from Point a to Point b.
	 *
	 * @param a Start
	 * @param b End
	 * @return The list of Points on the line from a to b including a and b
	 */
	public static List<Point> bresenham(final Point a, final Point b) {
		List<Point> line = new ArrayList<>();
		int dx = Math.abs(b.x - a.x);
		int dy = Math.abs(b.y - a.y);

		int sx = a.x < b.x ? 1 : -1;
		int sy = a.y < b.y ? 1 : -1;

		int err = dx - dy;
		int e2;

		Point current = a;

		while (true) {
			line.add(current);

			if (current.x == b.x && current.y == b.y) {
				break;
			}

			current = new Point(current);

			e2 = 2 * err;
			if (e2 > -dy) {
				err -= dy;
				current.x += sx;
			}

			if (e2 < dx) {
				err += dx;
				current.y += sy;
			}
		}
		return line;
	}

}
