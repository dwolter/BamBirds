/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab.shape;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.GeometryUtil;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.Rectangle;

public class Rect extends Body {
	private static final long serialVersionUID = 1L;
	// width and height of the rectangle
	private final Polygon polygon;
	private double pWidth;
	private double pLength;

	public Rect(final double xs, final double ys, final double w, final double h, final double theta, final ABType type) {
		super(xs, ys);
		if (h >= w) {
			setAngle(theta);
			pWidth = w;
			pLength = h;
		} else {
			setAngle(theta + Math.PI / 2);
			pWidth = h;
			pLength = w;
		}

		setArea((int) (pWidth * pLength));
		setType(type);

		polygon = GeometryUtil.createPolygon(getAngle(), getCenterX(), getCenterY(), pWidth, pLength);
		super.setBounds(getPolygon().getBounds());
		width = getPolygon().getBounds().width;
		height = getPolygon().getBounds().height;

	}

	public Rect(final int[] box, final ABType type) {
		super((box[0] + box[2]) / 2.0, (box[3] + box[1]) / 2.0);
		pWidth = box[2] - box[0];
		pLength = box[3] - box[1];
		setAngle(Math.PI / 2);

		if (pLength < pWidth) {
			pWidth = pLength;
			pLength = box[2] - box[0];
			setAngle(0);
		}

		width = (int) pWidth;
		height = (int) pLength;

		setType(type);

		setArea(width * height);

		polygon = GeometryUtil.createPolygon(getAngle(), getCenterX(), getCenterY(), pWidth, pLength);

	}

	public Rect(final double centerX, final double centerY, final double pWidth, final double pLength, final double angle, final ABType type, final int area) {
		super(centerX, centerY);
		this.pWidth = pWidth;
		this.pLength = pLength;
		setArea(area);
		setType(type);
		setAngle(angle);
		polygon = GeometryUtil.createPolygon(angle, centerX, centerY, pWidth, pLength);
		super.setBounds(getPolygon().getBounds());
		width = getPolygon().getBounds().width;
		height = getPolygon().getBounds().height;
	}

	public double getPWidth() {
		if (pWidth != -1) {
			return pWidth;
		}
		return width;
	}

	public double getPLength() {
		if (pLength != -1) {
			return pLength;
		}
		return height;
	}

	@Override
	public Rectangle getBounds() {
		return getPolygon().getBounds();
	}

	/* draw the rectangle onto canvas */
	@Override
	public void draw(final Graphics2D g, final boolean fill, final Color color, final double padding) {
		//TODO: padding not so easy with polygon
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
		return String.format("Rect: id:%s type:%s hollow:%b Area:%d w:%7.3f h:%7.3f a:%3.3f at x:%3.1f y:%3.1f", getGlobalID(),
				getType(), isHollow(), getArea(), pWidth, pLength, getAngle(), getCenterX(), getCenterY());
	}

	@Override
	public void translate(int x, int y) {
		super.translate(x, y);
		polygon.translate(x,y);
	}
}
