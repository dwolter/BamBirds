/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab.shape;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.ArrayList;

import de.uniba.sme.bambirds.common.objects.ab.ABShape;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.LineSegment;

public class Poly extends Body {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public Polygon polygon = null;

	public Poly(ArrayList<LineSegment> lines, int left, int top, ABType type, double xs, double ys) {
		polygon = new Polygon();
		shape = ABShape.Poly;
		if (lines != null) {
			for (LineSegment l : lines) {
				Point start = l._start;
				polygon.addPoint(start.x + left, start.y + top);
			}
		}
		centerX = xs;
		centerY = ys;
		angle = 0;
		area = getBounds().height * getBounds().width;
		this.type = type;
		super.setBounds(polygon.getBounds());
	}

	@Override
	public Rectangle getBounds() {
		return polygon.getBounds();
	}

	@Override
	public void draw(Graphics2D g, boolean fill, Color color, double padding) {
		//TODO: padding not so easy with polygon
		g.setColor(color);
		if (fill) {
			g.fillPolygon(polygon);
		} else {
			g.drawPolygon(polygon);
		}
	}

	public String toString() {
		return String.format("Poly: id:%d type:%s hollow:%b %dpts at x:%3.1f y:%3.1f", globalID, type, hollow,
				polygon.npoints, centerX, centerY);
	}
}
