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
import de.uniba.sme.bambirds.common.utils.MathUtil;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;

public class Circle extends Body {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	// radius of the circle
	private final double radius;

	/**
	 * Create a new circle.
	 *
	 * @param xs     - x coordinate of the circle centre
	 * @param ys     - y coordinate of the circle centre
	 * @param radius of the circle
	 * @param type   of Object
	 */
	public Circle(final double xs, final double ys, final double radius, final ABType type) {
		super(xs, ys);
		this.radius = radius;
		setShape(ABShape.Circle);
		Rectangle bounds = new Rectangle(
				(int) (xs - getRadius() * Math.sin(MathUtil.PI_4)),
				(int) (ys - getRadius() * Math.sin(MathUtil.PI_4)),
				(int) (2 * getRadius() * Math.sin(MathUtil.PI_4)),
				(int) (2 * getRadius() * Math.sin(MathUtil.PI_4))
		);
		setAngle(0);
		setType(type);
		setArea((int) (Math.PI * getRadius() * getRadius()));
		super.setBounds(bounds);
	}

	public Circle(final int[] box, final ABType type) {
		super((box[0] + box[2]) / 2.0,
				(box[1] + box[3]) / 2.0);
		radius = (box[2] - box[0] + box[3] - box[1]) / 4.0;
		setShape(ABShape.Circle);
		setArea((int) (Math.PI * getRadius() * getRadius()));
		Rectangle bounds = new Rectangle(
				MathUtil.round(getCenterX() - getRadius() * Math.sin(MathUtil.PI_4)),
				MathUtil.round(getCenterY() - getRadius() * Math.sin(MathUtil.PI_4)),
				MathUtil.round(2 * getRadius() * Math.sin(MathUtil.PI_4)),
				MathUtil.round(2 * getRadius() * Math.sin(MathUtil.PI_4))
		);
		setAngle(0);
		setType(type);
		super.setBounds(bounds);
	}

	/* draw the circle onto canvas */
	@Override
	public void draw(final Graphics2D g, final boolean fill, final Color color, final double padding) {
		g.setColor(color);
		if (fill) {
			g.fillOval(MathUtil.round(getCenterX() - getRadius() - padding), MathUtil.round(getCenterY() - getRadius() - padding), MathUtil.round(getRadius() * 2 + padding * 2),
					MathUtil.round(getRadius() * 2 + padding * 2));
		} else {
			g.drawOval(MathUtil.round(getCenterX() - getRadius() - padding), MathUtil.round(getCenterY() - getRadius() - padding), MathUtil.round(getRadius() * 2 + padding * 2),
					MathUtil.round(getRadius() * 2 + padding * 2));
		}
	}

	@Override
	public Polygon getPolygon() {
		Polygon p = new Polygon();

		// As the circle is converted to a hexagon, the non-easy calculatable vertices are offset differently than the others
		double edgeMargin = Math.sqrt((getRadius() * getRadius()) / 2);

		p.addPoint(MathUtil.round(getCenterX() - getRadius()), MathUtil.round(getCenterY()));                // 🡠
		p.addPoint(MathUtil.round(getCenterX() - edgeMargin), MathUtil.round(getCenterY() - edgeMargin)); // 🡤
		p.addPoint(MathUtil.round(getCenterX()), MathUtil.round(getCenterY() - getRadius()));               // 🡡
		p.addPoint(MathUtil.round(getCenterX() + edgeMargin), MathUtil.round(getCenterY() - edgeMargin)); // 🡥
		p.addPoint(MathUtil.round(getCenterX() + getRadius()), MathUtil.round(getCenterY()));               // 🡢
		p.addPoint(MathUtil.round(getCenterX() + edgeMargin), MathUtil.round(getCenterY() + edgeMargin)); // 🡦
		p.addPoint(MathUtil.round(getCenterX()), MathUtil.round(getCenterY() + getRadius()));                // 🡣
		p.addPoint(MathUtil.round(getCenterX() - edgeMargin), MathUtil.round(getCenterY() + edgeMargin)); // 🡧

		return p;
	}

	public String toString() {
		return String.format("Circ: id:%s type:%s r:%7.3f at x:%5.1f y:%5.1f", getGlobalID(), getType(), getRadius(), getCenterX(), getCenterY());
	}

	public double getRadius() {
		return radius;
	}
}
