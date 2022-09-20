package de.uniba.sme.bambirds.common.objects.ab;

import java.awt.Color;

/**
 * Colors
 */
public class Colors {

	public static final int[] DRAW_COLOR = new int[20];

	static {
		DRAW_COLOR[ABType.Background.id()] = 0xdddddd;
		DRAW_COLOR[ABType.Ground.id()] = 0x152053;
		DRAW_COLOR[ABType.Hill.id()] = 0x342213;
		DRAW_COLOR[ABType.Sling.id()] = 0x7f4120;
		DRAW_COLOR[ABType.Edge.id()] = 0x000000;
		DRAW_COLOR[ABType.Stone.id()] = 0xa0a0a0;
		DRAW_COLOR[ABType.Ice.id()] = 0x6ecdf8;
		DRAW_COLOR[ABType.Wood.id()] = 0xe09020;
		DRAW_COLOR[ABType.Pig.id()] = 0x60e048;
		DRAW_COLOR[ABType.Trajectory.id()] = 0xffffff;
		DRAW_COLOR[ABType.BlueBird.id()] = 0x60a8c0;
		DRAW_COLOR[ABType.RedBird.id()] = 0xd00028;
		DRAW_COLOR[ABType.YellowBird.id()] = 0xf0d820;
		DRAW_COLOR[ABType.BlackBird.id()] = 0x0f0f0f;
		DRAW_COLOR[ABType.WhiteBird.id()] = 0xe8e8c8;
		DRAW_COLOR[ABType.Duck.id()] = 0xf0d820;
		DRAW_COLOR[ABType.Watermelon.id()] = 0x80a818;
		DRAW_COLOR[ABType.TNT.id()] = Color.RED.getRGB();
	}

	public static Color get(final ABType type) {
		if (type == ABType.Unknown) {
			return new Color(0x000000);
		}
		return new Color(DRAW_COLOR[type.id()]);
	}

}