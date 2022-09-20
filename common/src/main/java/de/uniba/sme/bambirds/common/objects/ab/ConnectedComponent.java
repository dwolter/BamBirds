/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/

package de.uniba.sme.bambirds.common.objects.ab;

import de.uniba.sme.bambirds.common.objects.ab.shape.Body;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import de.uniba.sme.bambirds.common.utils.MathUtil;

import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

public class ConnectedComponent {

	public static final double HOLLOW_THRESHOLD = 1.2;
	public static final int RESOLUTION = 50;
	public static final double JOIN_THRESHOLD = 1.2; //1.4
	public static final int SMALL_SIZE = 14;
	// neighbour lookup table
	private static Point[][][] connectedPoint = null;
	private static boolean firstTime = true;

	// atan lookup table
	private static double[][] atan;
	public static final double ANGLE_UNDEFINED = 2 * Math.PI;
	private static final int WINDOW_SIZE = 10;

	// types of points in the image
	private static final int EMPTY = 0;
	private static final int FILLED = 1;
	private static final int EDGE = 2;

	// define small object as min(width, height) < constant
	private static final int SMALL = 20;

	// smoothing constant
	private static final int SMOOTH = 3;

	private static final double ANGLE_THRESHOLD_HILL = Math.toRadians(150);
	private static final double ANGLE_THRESHOLD_DEFAULT = Math.toRadians(85);

	// component parameters
	private final int area;
	private final int perimeter;
	private final int type;
	private final int[][] image;
	private int notActualType = 0;

	// points and lines in the component
	private ArrayList<LineSegment> lineSegments = null;
	private final ArrayList<Point> edgePoints;

	// size of the bounding box
	private final int left;
	private final int top;
	private final int width;
	private final int height;

	// extreme points
	private final Point[] extrema;

	/* Create a new connected component
	 * @param   map - class map of the screenshot
	 *          x,y - starting coordinate of the connected component
	 *          ignore - points on the map to ignore
	 *          isEightConnect - whether the component is eight-connected or four-connected
	 */
	public ConnectedComponent(final int[][] map, final int x, final int y, final boolean[][] ignore, final boolean isEightConnect) {
		// initialise the neighbour map
		initialise(map);

		// set object type and size
		type = map[y][x];
		extrema = new Point[4];
		Arrays.fill(extrema, new Point(x, y));

		// apply BFS to find all connected pixels
		boolean[][] searched = new boolean[map.length][map[0].length];
		Queue<Point> q = new LinkedList<>();
		q.add(new Point(x, y));
		searched[y][x] = true;

		int connectivity = isEightConnect ? 1 : 2;

		edgePoints = new ArrayList<>();
		ArrayList<Point> points = new ArrayList<>();
		while (!q.isEmpty()) {
			Point p = q.poll();

			if (map[p.y][p.x] == type
					|| (type == ABType.Ice.id() && map[p.y][p.x] == ABType.BlueBird.id())
					|| (type == ABType.WhiteBird.id() && map[p.y][p.x] == ABType.Trajectory.id())
			) {
				if (map[p.y][p.x] != type) {
					notActualType++;
				}
				points.add(p);
				ignore[p.y][p.x] = true;
				boolean added = false;
				for (int i = 0; i < 8; i += connectivity) {
					Point np = connectedPoint[p.y][p.x][i];

					// test for image boundaries
					if (np.x == p.x && np.y == p.y & !added) {
						edgePoints.add(p);
						added = true;
					}
					if (!searched[np.y][np.x]) {
						q.add(np);
						searched[np.y][np.x] = true;
					}
				}
			} else {
				// add point to edge
				edgePoints.add(p);
			}
		}

		// update extremas
		for (Point p : edgePoints) {
			if (p.x < extrema[0].x) {
				extrema[0] = p;
			}
			if (p.x > extrema[1].x) {
				extrema[1] = p;
			}
			if (p.y < extrema[2].y) {
				extrema[2] = p;
			}
			if (p.y > extrema[3].y) {
				extrema[3] = p;
			}
		}

		// set size and location
		top = extrema[2].y - 2;
		left = extrema[0].x - 2;
		height = extrema[3].y - top + 3;
		width = extrema[1].x - left + 3;
		area = points.size();

		for (int i = 0; i < 4; i++) {
			extrema[i] = new Point(extrema[i].x - left, extrema[i].y - top);
		}

		// generate the image
		image = new int[height][width];
		for (Point p : points) {
			image[p.y - top][p.x - left] = FILLED;
		}
		for (Point p : edgePoints) {
			image[p.y - top][p.x - left] = EDGE;
		}

		perimeter = edgePoints.size();
	}

	/* Trace the contour using Moore-Neighbour tracing
	 * and partition contour points into line segments
	 * @return  list of line segments which form a cycle around
	 *          outer border of the connected component
	 */
	private ArrayList<LineSegment> findLines() {
		double angleThreshold = type == ABType.Hill.id() ? ANGLE_THRESHOLD_HILL : ANGLE_THRESHOLD_DEFAULT;

		// tracing points for Moore-Neighbour algorithm
		Point current = null;
		Point prev = null;
		Point next;

		// search for starting point
		for (int x = 0; x < width && current == null; x++) {
			for (int y = 0; y < height; y++) {
				if (image[y][x] != EMPTY) {
					current = new Point(x, y);
					prev = new Point(x, y - 1);
					break;
				}
			}
		}

		Point[] path = new Point[perimeter];
		path[0] = current;
		int length = 1;

		// walk in the anticlockwise direction until initial point
		// is returned to
		next = clockwise(current, prev);
		while (!(next.equals(path[0]))) {
			if (image[next.y][next.x] != EMPTY) {
				current = next;
				next = prev;
				path[length] = current;
				length++;
			} else {
				prev = next;
				next = clockwise(current, next);
			}
		}

		// partition contour into line segments
		lineSegments = new ArrayList<>();
		LineSegment line = new LineSegment(path[0], ANGLE_UNDEFINED);
		for (int i = 1; i < length - SMOOTH; i++) {
			// approximate local angle by looking ahead
			Point p = path[i];
			Point ahead = path[i + SMOOTH];

			int yDiff = ahead.y - p.y;
			int xDiff = ahead.x - p.x;
			double angle = atan[yDiff + WINDOW_SIZE][xDiff + WINDOW_SIZE];

			// if point adding unsuccessful
			double change = line.addPoint(p, angle, angleThreshold);
			if (change != 0) {
				line.removeEndPoint();
				lineSegments.add(line);

				line = new LineSegment(path[i - 1], angle);
			}
		}
		lineSegments.add(line);

		// join lines with similar angle
		// join _lines with similar angle
		ArrayList<LineSegment> newlines = new ArrayList<>();
		LineSegment prevline = lineSegments.get(0);
		for (int i = 1; i < lineSegments.size(); i++) {
			if (!prevline.join(lineSegments.get(i))) {
				newlines.add(prevline);
				prevline = lineSegments.get(i);
			}
		}
		newlines.add(prevline);
		lineSegments = newlines;
		return lineSegments;
	}

	/* Determine if the connected component is a Rectangle,
	 * Circle or in fact multiple objects stacked together.
	 *
	 * @param   list of corners the component contains
	 *          (points where contour orientation changes)
	 * @return  Most likely shape of the object, null if it is noise
	 */
	private Body findShape(final List<Point> corners) {
		final double res = Math.PI / 2 / RESOLUTION;
		double width = 0;
		double height = 0;
		double angle = 0;
		double areaMin = 99999;
		double areaMax = 0;

		for (int i = 0; i < RESOLUTION; i++) {
			double theta = i * res;
			double min1 = Integer.MAX_VALUE;
			double min2 = Integer.MAX_VALUE;
			double max1 = Integer.MIN_VALUE;
			double max2 = Integer.MIN_VALUE;

			// rotate each point about the origin and record min/max x,y coordinates
			for (Point p : corners) {
				double p1 = p.x * Math.cos(theta) + p.y * Math.sin(theta);
				double p2 = p.x * Math.sin(theta) - p.y * Math.cos(theta);

				if (p1 < min1) {
					min1 = p1;
				}
				if (p1 > max1) {
					max1 = p1;
				}
				if (p2 < min2) {
					min2 = p2;
				}
				if (p2 > max2) {
					max2 = p2;
				}
			}

			// width and height are calculated as difference between
			// min and max distances
			double h = max1 - min1;
			double w = max2 - min2;
			double a = w * h;

			if (a < areaMin) {
				height = h;
				width = w;
				angle = theta;
				areaMin = a;
			}
			if (a > areaMax) {
				areaMax = a;
			}
		}

		// relax circle threshold for objects with larger size
		// and objects which are lying at angles close to 45 degrees
		double tc;
		if (Math.max(width, height) > SMALL_SIZE || Math.abs(angle - MathUtil.PI_4) < MathUtil.PI_8) {
			tc = 1.2; //1.3
		} else {
			tc = 1.1;
		}

		double x = left + (this.width / 2.0) - 0.5;
		double y = top + (this.height / 2.0) - 0.5;

		// test for noise
		if (width <= 3 || height <= 3) {
			return null;
		}


		Poly poly = new Poly(findLines(), left, top, assignType(type), left + this.width / 2f, top + this.height / 2f);
		// Objects with less than 3 points are noise
		if (poly.getPolygon().npoints < 3) {
			return null;
		}
		int polyArea = getArea(poly.getPolygon());
		int actualArea = Math.max((area + perimeter), polyArea);

		// test for joined component

		//if (areaMin > (_area + _perimeter) * JOIN_THRESHOLD)
		if (areaMin > actualArea * JOIN_THRESHOLD) {

			if ((area + perimeter) <= 400) {
				if (polyArea > (area) * 1.1) {
					poly.setHollow(true);
				}
			}
			return poly;

		}
		// test for circle
		if (areaMin * tc > areaMax && Math.abs(this.width - this.height) <= 3) {
			int r = (this.width + this.height) / 4 - 2;
			return new Circle(x, y, r, assignType(type));
		}
		Rect rect = new Rect(x, y, width, height, angle, assignType(type));
		if (areaMin > (area + perimeter) * HOLLOW_THRESHOLD) {
			rect.setHollow(true);
		}
		return rect;
	}


	public int getArea(final Polygon poly) {
		int area = 0;
		Rectangle rect = poly.getBounds();
		for (int x = rect.x; x < rect.x + rect.width; x++) {
			for (int y = rect.y; y < rect.y + rect.height; y++) {
				if (poly.contains(x, y)) {
					area++;
				}
			}
		}
		return area;

	}
	//public boolean testHollow(Shape shape){}

	/* find the most likely shape of the component
	 * @return  most likely shape, null if it is noise
	 */
	public Body getBody() {
		if (type == ABType.Sling.id()) {
			return new Rect(boundingBox(), assignType(type));
		}

		if ((type > ABType.Sling.id()
				&& type <= ABType.Pig.id())
				|| type == ABType.Trajectory.id()) {
			return new Circle(boundingBox(), assignType(type));
		}

		if (type == ABType.Hill.id()) {
			return new Poly(findLines(), left, top, assignType(type), left + width / 2f, top + height / 2f);
		}

		ArrayList<Point> corners = new ArrayList<>();
		// use all edge points if the shape is small
		if (width < SMALL || height < SMALL) {
			corners = edgePoints;
		} else {
			// otherwise, find corners first by border tracking
			findLines();

			for (LineSegment line : lineSegments) {
				corners.add(line.getStart());
				corners.add(line.getEnd());
			}
			for (Point p : extrema) {
				if (!corners.contains(p)) {
					corners.add(p);
				}
			}
		}
		return findShape(corners);
	}


	// return number of internal points in the component
	public int getArea() {
		return area;
	}

	// number of border points
	public int getType() {
		return type;
	}

	/* the bounding box {left, top, right, bottom} */
	public int[] boundingBox() {
		return new int[]{left + 2, top + 2, left + width - 3, top + height - 3};
	}

	/* draw the connected component
	 * @param   canvas
	 *          drawEdge - if border should be drawn
	 *          drawCorner - if the corner points should be drawn
	 */
	public void draw(final BufferedImage canvas, final boolean drawEdge, final boolean drawCorners) {
		for (int y = 2; y < height - 2; y++) {
			for (int x = 2; x < width - 2; x++) {
				if (image[y][x] == FILLED) {
					canvas.setRGB(x + left, y + top, Colors.DRAW_COLOR[type]);
				} else if (drawEdge && image[y][x] == EDGE) {
					canvas.setRGB(x + left, y + top, 0x000000);
				}
			}
		}


		if (drawCorners) {
			findLines();
			if (lineSegments != null) {
				for (LineSegment line : lineSegments) {
					line.draw(canvas.createGraphics(), left, top);
				}
			}
		}
	}


	/* initialise neighbour and atan lookup table
	 * @param   class map of the current game
	 */
	private static void initialise(final int[][] map) {
		if (connectedPoint != null
				&& connectedPoint.length == map.length
				&& connectedPoint[0].length == map[0].length) {
			return;
		}

		// initialise atan lookup
		if (firstTime) {
			atan = new double[WINDOW_SIZE * 2][WINDOW_SIZE * 2];

			for (int y = 0; y < 2 * WINDOW_SIZE; y++) {
				for (int x = 0; x < 2 * WINDOW_SIZE; x++) {
					if (x - WINDOW_SIZE == 0 && y - WINDOW_SIZE == 0) {
						atan[y][x] = ANGLE_UNDEFINED;
					} else if (x - WINDOW_SIZE == 0) {
						atan[y][x] = Math.PI / 2;
					} else {
						atan[y][x] = Math.atan((double) (y - WINDOW_SIZE) / (x - WINDOW_SIZE));

						if (atan[y][x] < 0) {
							atan[y][x] += Math.PI;
						}
					}
				}
			}
			firstTime = false;
		}

		// initialise neighbour point map
		int width = map[0].length;
		int height = map.length;
		connectedPoint = new Point[height][width][8];
		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				connectedPoint[y][x][0] = new Point(x, y - 1);
				connectedPoint[y][x][1] = new Point(x + 1, y - 1);
				connectedPoint[y][x][2] = new Point(x + 1, y);
				connectedPoint[y][x][3] = new Point(x + 1, y + 1);
				connectedPoint[y][x][4] = new Point(x, y + 1);
				connectedPoint[y][x][5] = new Point(x - 1, y + 1);
				connectedPoint[y][x][6] = new Point(x - 1, y);
				connectedPoint[y][x][7] = new Point(x - 1, y - 1);
				for (int i = 0; i < 8; i++) {
					Point p = connectedPoint[y][x][i];
					if (p.x >= width || p.y >= height || p.x < 0 || p.y < 0) {
						connectedPoint[y][x][i] = new Point(x, y);
					}
				}
			}
		}
	}

	public ABType assignType(final int visionType) {
		return ABType.fromID(visionType);
	}

	private static final int[][] X_CLOCK = {{0, -1, -1},
			{0, 0, 0},
			{1, 1, 0}};
	private static final int[][] Y_CLOCK = {{1, 0, 0},
			{1, 0, -1},
			{0, 0, -1}};

	/* find the next point to trace in Moore-Neighbourhood
	 * @param   p - the current contour point
	 *          prev - point just examined
	 * @return  point connected to p which is in the anticlockwise
	 *          direction from p to prev
	 */
	private static Point clockwise(final Point p, final Point prev) {
		int dx = prev.x - p.x + 1;
		int dy = prev.y - p.y + 1;
		return new Point(prev.x + X_CLOCK[dy][dx], prev.y + Y_CLOCK[dy][dx]);
	}

	public double getTypeRatio() {
		return (double) (area - notActualType) / area;
	}
}
