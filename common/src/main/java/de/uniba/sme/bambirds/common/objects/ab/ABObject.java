/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab;

import java.awt.Color;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.utils.GeometryUtil;

public class ABObject extends Rectangle {
	private static final Logger log = LogManager.getLogger();
	private static final long serialVersionUID = 1L;
	private static int counter = 0;

	// Field for the id
	public int id;

	// Field for the global ID
	public String globalID;

	// object type
	public ABType type;

	public int area = 0;
	// For all MBRs, the shape is Rect by default.
	public ABShape shape = ABShape.Rect;

	// For all MBRs, the angle is 0 by default.
	public double angle = 0;

	// is Hollow or not
	public boolean hollow = false;

	public ABObject(Rectangle mbr, ABType type) {
		this(mbr, type, counter++);
	}

	public ABObject(Rectangle mbr, ABType type, int id) {
		super(mbr);
		this.type = type;
		this.id = id;
	}

	public ABObject(ABObject ab) {
		this(ab.getBounds(),ab.type, ab.id);
	}

	public ABObject() {
		this(new Rectangle(),ABType.Unknown, counter++);
	}

	public ABType getType() {
		return type;
	}
	
	public Point getCenter() {
		return new Point((int) getCenterX(), (int) getCenterY());
	}

	public static void resetCounter() {
		counter = 0;
	}
	
	public static int round(double i) {
		return (int) (i + 0.5);
	}

	/**
	 * Get the all the vertices for the object
	 * @return a list of Points
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
	 * {@link ABType} and the respective Value in {@link Colors}
	 * 
	 * @param g Graphics2D Object to draw on
	 */
	public void draw(Graphics2D g) {
		draw(g, false, Colors.get(type), 0);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}
	 * 
	 * @param g    Graphics2D Object to draw on
	 * @param fill if the Object should be drawn filled or outlined
	 */
	public void draw(Graphics2D g, boolean fill) {
		draw(g, fill, Colors.get(type), 0);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}
	 * 
	 * @param g     Graphics2D Object to draw on
	 * @param color The Color to draw the Object with
	 */
	public void draw(Graphics2D g, Color color) {
		draw(g, false, color, 0);
	}

	/**
	 * Draws the Outline of the Object with the Color specified by the
	 * {@link ABType} and the respective Value in {@link Colors}
	 * 
	 * @param g       Graphics2D Object to draw on
	 * @param padding of the drawn output
	 */
	public void draw(Graphics2D g, double padding) {
		draw(g, false, Colors.get(type), padding);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}
	 * 
	 * @param g       Graphics2D Object to draw on
	 * @param fill    if the Object should be drawn filled or outlined
	 * @param padding of the drawn output
	 */
	public void draw(Graphics2D g, boolean fill, double padding) {
		draw(g, fill, Colors.get(type), padding);
	}

	/**
	 * Draws the Object with the Color specified by the {@link ABType} and the
	 * respective Value in {@link Colors}
	 * 
	 * @param g       Graphics2D Object to draw on
	 * @param color   The Color to draw the Object with
	 * @param padding of the drawn output
	 */
	public void draw(Graphics2D g, Color color, double padding) {
		draw(g, false, color, padding);
	}

	/**
	 * Draws the Object
	 * 
	 * @param g     Graphics2D Object to draw on
	 * @param fill  if the Object should be drawn filled or outlined
	 * @param color The Color to draw the Object with
	 */
	public void draw(Graphics2D g, boolean fill, Color color) {
		draw(g, fill, color, 0);
	}

	/**
	 * Draws the Object
	 * 
	 * @param g       Graphics2D Object to draw on
	 * @param fill    if the Object should be drawn filled or outlined
	 * @param color   The Color to draw the Object with
	 * @param padding of the drawn output
	 */
	public void draw(Graphics2D g, boolean fill, Color color, double padding) {
		g.setColor(color);
		Rectangle rect = new Rectangle(round(getX() - padding), round(getY() - padding), round(getWidth() + padding * 2),
				round(getHeight() + padding * 2));
		if (fill) {
			g.fill(rect);
		} else {
			g.draw(rect);
		}
	}

	public void drawRep(Graphics2D g){
		g.setColor(new Color(type.id,0,0));
		Polygon p = getPolygon();
		int[] xpoints = p.xpoints;
		int[] ypoints = p.ypoints;
		int npoints = p.npoints;
		g.fillPolygon(p);
		List<Point> norms = GeometryUtil.listOfNormsInt(p);
		for(int i = 0; i < npoints; i++) {
			Point currentPoint = norms.get(i);
			g.setColor(new Color(type.id,currentPoint.x,currentPoint.y));
			g.drawLine(xpoints[i], ypoints[i], xpoints[(i + 1) % npoints], ypoints[(i + 1) % npoints]);
		}
	}

}
