/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects.ab;

public enum ABType {

	Background(0),
	Ground(1),
	Hill(2),
	Sling(3),
	RedBird(4), 
	YellowBird(5), 
	BlueBird(6), 
	BlackBird(7), 
	WhiteBird(8), 
	Pig(9),
	Ice(10), 
	Wood(11), 
	Stone(12), 
	Duck(13),
	Edge(14),
	Watermelon(15),
	Trajectory(16),
	TNT(18),
	Unknown(-1); //TODO: This has changed from 0 to -1. Maybe there are some classes checking for 0
	public int id;
	private ABType(int id)
	{
		this.id = id;
	}

    public static ABType fromObjectID(String objectId) {
		if (objectId == null) return Unknown;
		if (objectId.startsWith("redbird")) {
			return RedBird;
		} else if (objectId.startsWith("yellowbird")) {
			return YellowBird;
		} else if (objectId.startsWith("bluebird")) {
			return BlueBird;
		} else if (objectId.startsWith("blackbird")) {
			return BlackBird;
		} else if (objectId.startsWith("whitebird")) {
			return WhiteBird;
		} else if (objectId.startsWith("pig")) {
			return Pig;
		} else if (objectId.startsWith("ice")) {
			return Ice;
		} else if (objectId.startsWith("wood")) {
			return Wood;
		} else if (objectId.startsWith("stone")) {
			return Stone;
		} else if (objectId.startsWith("tnt")) {
			return TNT;
		}
		return Unknown;
    }

    public int id() {
		return this.id;
	}

	public boolean isBird() {
		return id >= 4 && id <= 8;
	}

	public static ABType fromID(int id) {
		switch (id) {
			case 0:
				return ABType.Background;
			case 1:
				return ABType.Ground;
			case 2:
				return ABType.Hill;
			case 3:
				return ABType.Sling;
			case 4:
				return ABType.RedBird;
			case 5:
				return ABType.YellowBird;
			case 6:
				return ABType.BlueBird;
			case 7:
				return ABType.BlackBird;
			case 8:
				return ABType.WhiteBird;
			case 9:
				return ABType.Pig;
			case 10:
				return ABType.Ice;
			case 11:
				return ABType.Wood;
			case 12:
				return ABType.Stone;
			case 13:
				return ABType.Duck;
			case 14:
				return ABType.Edge;
			case 15:
				return ABType.Watermelon;
			case 16:
				return ABType.Trajectory;
			case 18:
				return ABType.TNT;
			default:
				return ABType.Unknown;
		}
	}
}
