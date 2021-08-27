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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.GeometryUtil;

public class Rect extends Body {
	private static final Logger log = LogManager.getLogger();
	private static final long serialVersionUID = 1L;
	// width and height of the rectangle
	public Polygon polygon;
	protected double pwidth = -1, plength = -1;
	
	public Rect(double xs, double ys, double w, double h, double theta, ABType type) {
		
		if (h >= w) {
			angle = theta;
			pwidth = w;
			plength = h;
		} else {
			angle = theta + Math.PI / 2;
			pwidth = h;
			plength = w;
		}
		
		centerY = ys;
		centerX = xs;
		
		area = (int) (pwidth * plength);
		this.type = type;
		
		polygon = GeometryUtil.createPolygon(angle,centerX,centerY,pwidth,plength);
		super.setBounds(polygon.getBounds());
		width = polygon.getBounds().width;
		height = polygon.getBounds().height;
		
	}
	
	public Rect(int box[], ABType type) {
		centerX = (box[0] + box[2]) / 2.0;
		centerY = (box[3] + box[1]) / 2.0;
		pwidth = box[2] - box[0];
		plength = box[3] - box[1];
		angle = Math.PI / 2;
		
		if (plength < pwidth) {
			pwidth = plength;
			plength = box[2] - box[0];
			angle = 0;
		}
		
		width = (int) pwidth;
		height = (int) plength;
		
		this.type = type;
		
		area = width * height;
		polygon = GeometryUtil.createPolygon(angle,centerX,centerY,pwidth,plength);
		
	}
	
	public Rect(double centerX, double centerY, double pwidth, double plength, double angle, ABType type, int area) {
		this.centerX = centerX;
		this.centerY = centerY;
		this.pwidth = pwidth;
		this.plength = plength;
		this.type = type;
		this.angle = angle;
		this.area = area;
		polygon = GeometryUtil.createPolygon(angle,centerX,centerY,pwidth,plength);
		super.setBounds(polygon.getBounds());
		width = polygon.getBounds().width;
		height = polygon.getBounds().height;
		
	}
	
	public double getpWidth() {
		if (pwidth != -1)
			return pwidth;
		return width;
	}

	public double getpLength() {
		if (plength != -1)
			return plength;
		return height;
	}
	
	@Override
	public Rectangle getBounds() {
		return polygon.getBounds();
	}
	
	/* draw the rectangle onto canvas */
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

	@Override
	public Polygon getPolygon() {
		return polygon;
	}

	public String toString() {
		return String.format("Rect: id:%d type:%s hollow:%b Area:%d w:%7.3f h:%7.3f a:%3.3f at x:%3.1f y:%3.1f", globalID,
				type, hollow, area, pwidth, plength, angle, centerX, centerY);
	}

}
