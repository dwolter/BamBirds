/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab.shape;

import java.awt.Point;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;

public abstract class Body extends ABObject

{
	private static final long serialVersionUID = 1L;

	public Body() {
		super();
	}

	// position (x, y) as center of the object
	public double centerX = 0;
	public double centerY = 0;

	@Override
	public Point getCenter() {
		Point point = new Point();
		point.setLocation(centerX, centerY);
		return point;
	}

	@Override
	public double getCenterX() {
		return centerX;
	}

	@Override
	public double getCenterY() {
		return centerY;
	}

}
