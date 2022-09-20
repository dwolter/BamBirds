/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab.shape;

import de.uniba.sme.bambirds.common.objects.ab.ABShape;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.LineSegment;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.List;

public class Poly extends Body {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	private final Polygon polygon;

	/**
	 * @param lines that make up the polygon
	 * @param left  offset in x direction
	 * @param top   offset in y direction
	 * @param type  Object Type
	 * @param xs    x coordinate for centerpoint
	 * @param ys    y coordinates for centerpoint
	 */
	public Poly(final List<LineSegment> lines, final int left, final int top, final ABType type, final double xs, final double ys) {
		super(xs, ys);
		polygon = new Polygon();
		setShape(ABShape.Poly);
		if (lines != null) {
			for (LineSegment l : lines) {
				Point start = l.getStart();
				getPolygon().addPoint(start.x + left, start.y + top);
			}
		}
		setAngle(0);
		setArea(getBounds().height * getBounds().width);
		setType(type);
		super.setBounds(getPolygon().getBounds());
	}

	@Override
	public Rectangle getBounds() {
		return getPolygon().getBounds();
	}

	@Override
	public void draw(final Graphics2D g, final boolean fill, final Color color, final double padding) {
		// TODO: padding not so easy with polygon
		g.setColor(color);
		if (fill) {
			g.fillPolygon(getPolygon());
		} else {
			g.drawPolygon(getPolygon());
		}
	}

	@Override
	public Polygon getPolygon() {
		return polygon;
	}

	@Override
	public String toString() {
		return String.format("Poly: id:%s type:%s hollow:%b %dpts at x:%3.1f y:%3.1f", getGlobalID(), getType(), isHollow(),
				getPolygon().npoints, getCenterX(), getCenterY());
	}

	@Override
	public void translate(int x, int y) {
		super.translate(x, y);
		polygon.translate(x,y);
	}
}
