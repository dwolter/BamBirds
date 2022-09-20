/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab;

import de.uniba.sme.bambirds.common.utils.GeometryUtil;
import de.uniba.sme.bambirds.common.utils.MathUtil;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.List;

public class ABObject extends Rectangle implements Comparable<ABObject> {
	private static final Logger LOG = LogManager.getLogger();
	private static final long serialVersionUID = 1L;
	private static int counter = 0;

	// Field for the id
	private final int id;

	// Field for the global ID
	private String globalID;

	// object type
	private ABType type;

	private int area = 0;

	// For all MBRs, the shape is Rect by default.
	private ABShape shape = ABShape.Rect;

	// For all MBRs, the angle is 0 by default.
	private double angle = 0;

	// is Hollow or not
	private boolean hollow = false;

	public ABObject(final Rectangle mbr, final ABType type) {
		this(mbr, type, counter++);
	}

	public ABObject(final Rectangle mbr, final ABType type, final int id) {
		super(mbr);
		this.area = mbr.height * mbr.width;
		this.type = type;
		this.id = id;
	}

	private ABObject(final Rectangle mbr, final ABType type, final int id, final String globalID, final int area, final ABShape shape, final double angle, final boolean hollow) {
		super(mbr);
		this.type = type;
		this.id = id;
		this.globalID = globalID;
		this.area = area;
		this.shape = shape;
		this.angle = angle;
		this.hollow = hollow;
	}


	public ABObject(final ABObject ab) {
		this(ab.getBounds(), ab.getType(), ab.getId(), ab.getGlobalID(), ab.getArea(), ab.getShape(), ab.getAngle(), ab.isHollow());
	}

	public ABObject() {
		this(new Rectangle(), ABType.Unknown, counter++);
	}

	public static void resetCounter() {
		counter = 0;
	}


	/**
	 * Get the all the vertices for the object as Polygon.
	 *
	 * @return a Polygon
	 */
	public Polygon getPolygon() {
		Polygon p = new Polygon();
		p.addPoint(this.x, this.y);
		p.addPoint(this.x, this.y);
		p.addPoint(this.x + this.width, this.y);
		p.addPoint(this.x + this.width, this.y + this.height);
		p.addPoint(this.x, this.y + this.height);
		return p;
	}

	/**
	 * Draws the Outline of the Object with the Color specified by the
	 * {@link ABType} and the respective Value in {@link Colors}.
	 *
	 * @param g Graphics2D Object to draw on
	 */
	public void draw(final Graphics2D g) {
		draw(g, false, Colors.get(getType()), 0);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}.
	 *
	 * @param g    Graphics2D Object to draw on
	 * @param fill if the Object should be drawn filled or outlined
	 */
	public void draw(final Graphics2D g, final boolean fill) {
		draw(g, fill, Colors.get(getType()), 0);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}.
	 *
	 * @param g     Graphics2D Object to draw on
	 * @param color The Color to draw the Object with
	 */
	public void draw(final Graphics2D g, final Color color) {
		draw(g, false, color, 0);
	}

	/**
	 * Draws the Outline of the Object with the Color specified by the
	 * {@link ABType} and the respective Value in {@link Colors}.
	 *
	 * @param g       Graphics2D Object to draw on
	 * @param padding of the drawn output
	 */
	public void draw(final Graphics2D g, final double padding) {
		draw(g, false, Colors.get(getType()), padding);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}.
	 *
	 * @param g       Graphics2D Object to draw on
	 * @param fill    if the Object should be drawn filled or outlined
	 * @param padding of the drawn output
	 */
	public void draw(final Graphics2D g, final boolean fill, final double padding) {
		draw(g, fill, Colors.get(getType()), padding);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}.
	 *
	 * @param g       Graphics2D Object to draw on
	 * @param color   The Color to draw the Object with
	 * @param padding of the drawn output
	 */
	public void draw(final Graphics2D g, final Color color, final double padding) {
		draw(g, false, color, padding);
	}

	/**
	 * Draws the Object.
	 *
	 * @param g     Graphics2D Object to draw on
	 * @param fill  if the Object should be drawn filled or outlined
	 * @param color The Color to draw the Object with
	 */
	public void draw(final Graphics2D g, final boolean fill, final Color color) {
		draw(g, fill, color, 0);
	}

	/**
	 * Draws the Object.
	 *
	 * @param g       Graphics2D Object to draw on
	 * @param fill    if the Object should be drawn filled or outlined
	 * @param color   The Color to draw the Object with
	 * @param padding of the drawn output
	 */
	public void draw(final Graphics2D g, final boolean fill, final Color color, final double padding) {
		g.setColor(color);
		Rectangle rect = new Rectangle(MathUtil.round(getX() - padding), MathUtil.round(getY() - padding), MathUtil.round(getWidth() + padding * 2),
				MathUtil.round(getHeight() + padding * 2));
		if (fill) {
			g.fill(rect);
		} else {
			g.draw(rect);
		}
	}

	public void drawRep(final Graphics2D g) {
		g.setColor(new Color(getType().id(), 0, 0));
		Polygon p = getPolygon();
		int[] xPoints = p.xpoints;
		int[] yPoints = p.ypoints;
		int nPoints = p.npoints;
		g.fillPolygon(p);
		List<Point> norms = GeometryUtil.listOfNormsInt(p);
		for (int i = 0; i < nPoints; i++) {
			Point currentPoint = norms.get(i);
			g.setColor(new Color(getType().id(), currentPoint.x, currentPoint.y));
			g.drawLine(xPoints[i], yPoints[i], xPoints[(i + 1) % nPoints], yPoints[(i + 1) % nPoints]);
		}
	}

	public int getId() {
		return id;
	}

	public ABType getType() {
		return type;
	}

	public Point getCenter() {
		return new Point((int) getCenterX(), (int) getCenterY());
	}

	public String getGlobalID() {
		return globalID;
	}

	public int getArea() {
		return area;
	}

	public ABShape getShape() {
		return shape;
	}

	public double getAngle() {
		return angle;
	}

	public boolean isHollow() {
		return hollow;
	}

	protected void setArea(final int area) {
		this.area = area;
	}

	public void setHollow(final boolean hollow) {
		this.hollow = hollow;
	}

	protected void setShape(final ABShape shape) {
		this.shape = shape;
	}

	protected void setAngle(final double angle) {
		this.angle = angle;
	}

	public void setType(final ABType type) {
		this.type = type;
	}

	public void setGlobalID(final String globalID) {
		this.globalID = globalID;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof ABObject) {
			ABObject abObject = (ABObject) obj;
			return abObject.type == this.type
					&& abObject.shape == this.shape
					&& MathUtil.isClose(abObject.angle, this.angle, 0.1)
					&& MathUtil.isClose(abObject.getCenterX(), this.getCenterX(), 1)
					&& MathUtil.isClose(abObject.getCenterY(), this.getCenterY(), 1);
		}
		return false;
	}

	@Override
	public int compareTo(ABObject abObject) {
		if (equals(abObject)) {
			return 0;
		}
		if (abObject.type != this.type) {
			return abObject.type.id() - this.type.id();
		}
		if (abObject.shape != this.shape) {
			return abObject.shape.ordinal() - this.shape.ordinal();
		}
		if (!MathUtil.isClose(abObject.getCenterX(), this.getCenterX(), 1)) {
			double difference = abObject.getCenterX() - this.getCenterX();
			return (int) Math.copySign(Math.ceil(Math.abs(difference)), difference);
		}
		if (!MathUtil.isClose(abObject.getCenterY(), this.getCenterY(), 1)) {
			double difference = abObject.getCenterY() - this.getCenterY();
			return (int) Math.copySign(Math.ceil(Math.abs(difference)), difference);
		}
		if (!MathUtil.isClose(abObject.angle, this.angle, 0.1)) {
			double difference = abObject.getCenterY() - this.getCenterY();
			return (int) Math.copySign(Math.ceil(Math.abs(difference)), difference);
		}
		return 0;
	}
}
