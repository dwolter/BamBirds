package de.uniba.sme.bambirds.common.objects.ab;

import java.awt.Color;

/**
 * Colors
 */
public class Colors {

	public static int _drawColor[] = new int[20];

	static {
		_drawColor[ABType.Background.id()] = 0xdddddd;
		_drawColor[ABType.Ground.id()] = 0x152053;
		_drawColor[ABType.Hill.id()] = 0x342213;
		_drawColor[ABType.Sling.id()] = 0x7f4120;
		_drawColor[ABType.Edge.id()] = 0x000000;
		_drawColor[ABType.Stone.id()] = 0xa0a0a0;
		_drawColor[ABType.Ice.id()] = 0x6ecdf8;
		_drawColor[ABType.Wood.id()] = 0xe09020;
		_drawColor[ABType.Pig.id()] = 0x60e048;
		_drawColor[ABType.Trajectory.id()] = 0xffffff;
		_drawColor[ABType.BlueBird.id()] = 0x60a8c0;
		_drawColor[ABType.RedBird.id()] = 0xd00028;
		_drawColor[ABType.YellowBird.id()] = 0xf0d820;
		_drawColor[ABType.BlackBird.id()] = 0x0f0f0f;
		_drawColor[ABType.WhiteBird.id()] = 0xe8e8c8;
		_drawColor[ABType.Duck.id()] = 0xf0d820;
		_drawColor[ABType.Watermelon.id()] = 0x80a818;
		_drawColor[ABType.TNT.id()] = Color.RED.getRGB();
	}

	public static Color get(ABType type){
		if (type == ABType.Unknown){
			return new Color(0x000000);
		}
		return new Color(_drawColor[type.id()]);
	}

	public static final int MAX_DIST = 100;
}