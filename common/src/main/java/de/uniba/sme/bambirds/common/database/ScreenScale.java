package de.uniba.sme.bambirds.common.database;

import java.awt.geom.Point2D;

import static de.uniba.sme.bambirds.common.objects.ab.ABType.BlackBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.BlueBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.RedBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.WhiteBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.YellowBird;

@SuppressWarnings("checkstyle:MagicNumber")
public final class ScreenScale {
	public static final RectSize SLINGSHOT = new RectSize(65, 201); // 66, 206
	public static final CircleSize[] BIRDS = new CircleSize[10];

	static {
		BIRDS[RedBird.id()] = new CircleSize(49, 47, 17.0, 5.0, 7.0);     // 45, 44
		BIRDS[BlueBird.id()] = new CircleSize(32, 31, 12.0, 2.0, 3.0);    // 30, 29
		BIRDS[BlackBird.id()] = new CircleSize(67, 89, 20.0, 3.0, 15.0);  // 64, 86
		BIRDS[YellowBird.id()] = new CircleSize(67, 54, 16.0, 7.0, 8.0);  // 56, 53
		BIRDS[WhiteBird.id()] = new CircleSize(86, 106, 26.0, 9.0, 16.0); // 80, 94
	}

	private ScreenScale() {
	}

	public static class RectSize {
		private final double width;
		private final double height;

		public RectSize(final double w, final double h) {
			width = w;
			height = h;
		}

		public double ratioW7H() {
			return getWidth() / getHeight();
		}

		public double ratioH7W() {
			return getHeight() / getWidth();
		}

		public double getWidth() {
			return width;
		}

		public double getHeight() {
			return height;
		}
	}

	public static class CircleSize extends RectSize {
		private final double radius;
		private final Point2D.Double offset;

		public CircleSize(final double w, final double h, final double r, final double offX, final double offY) {
			super(w, h);
			radius = r;
			offset = new Point2D.Double(offX, offY);
		}

		public double getRadius() {
			return radius;
		}

		public Point2D.Double getOffset() {
			return offset;
		}
	}
}
