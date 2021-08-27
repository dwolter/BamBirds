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
import java.awt.Rectangle;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.objects.ab.ABShape;
import de.uniba.sme.bambirds.common.objects.ab.ABType;

public class Circle extends Body {
	private static final Logger log = LogManager.getLogger();

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// radius of the circle
	public double r;

	/**
	 * Create a new circle
	 * @param xs
	 * @param ys - coordinate of the circle centre
	 * @param radius of the circle
	 * @param type of Object
	 */
	public Circle(double xs, double ys, double radius, ABType type) {

		centerX = xs;
		centerY = ys;
		r = radius;
		shape = ABShape.Circle;
		Rectangle bounds = new Rectangle((int) (xs - r * Math.sin(Math.PI / 4)), (int) (ys - r * Math.sin(Math.PI / 4)),
				(int) (2 * r * Math.sin(Math.PI / 4)), (int) (2 * r * Math.sin(Math.PI / 4)));
		this.type = type;
		angle = 0;
		area = (int) (Math.PI * r * r);
		super.setBounds(bounds);
	}

	public Circle(int box[], ABType type) {
		centerX = (box[0] + box[2]) / 2.0;
		centerY = (box[1] + box[3]) / 2.0;
		r = (box[2] - box[0] + box[3] - box[1]) / 4.0;
		shape = ABShape.Circle;
		area = (int) (Math.PI * r * r);
		Rectangle bounds = new Rectangle(round(centerX - r * Math.sin(Math.PI / 4)), round(centerY - r * Math.sin(Math.PI / 4)),
				round(2 * r * Math.sin(Math.PI / 4)), round(2 * r * Math.sin(Math.PI / 4)));
		angle = 0;
		this.type = type;
		super.setBounds(bounds);
	}

	/* draw the circle onto canvas */
	@Override
	public void draw(Graphics2D g, boolean fill, Color color, double padding) {
		g.setColor(color);
		if (fill) {
			g.fillOval(round(centerX - r - padding), round(centerY - r - padding), round(r * 2 + padding * 2),
					round(r * 2 + padding * 2));
		} else {
			g.drawOval(round(centerX - r - padding), round(centerY - r - padding), round(r * 2 + padding * 2),
					round(r * 2 + padding * 2));
		}
	}

	@Override
	public Polygon getPolygon() {
		Polygon p = new Polygon();

		// As the circle is converted to a hexagon, the non easy calculatable vertices are offset differently than the others
		double edge_margin = Math.sqrt((r*r)/2);

		p.addPoint(round(centerX-r),round(centerY));                       // 🡠
		p.addPoint(round(centerX-edge_margin),round(centerY-edge_margin)); // 🡤
		p.addPoint(round(centerX),round(centerY-r));                     // 🡡
		p.addPoint(round(centerX+edge_margin),round(centerY-edge_margin)); // 🡥
		p.addPoint(round(centerX+r),round(centerY));                       // 🡢
		p.addPoint(round(centerX+edge_margin),round(centerY+edge_margin)); // 🡦
		p.addPoint(round(centerX),round(centerY+r));                     // 🡣
		p.addPoint(round(centerX-edge_margin),round(centerY+edge_margin)); // 🡧

		return p;
	}

	public String toString() {
		return String.format("Circ: id:%d type:%s r:%7.3f at x:%5.1f y:%5.1f", globalID, type, r, centerX, centerY);
	}
}
