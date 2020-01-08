package database;

import java.awt.geom.Point2D;

import static ab.vision.ABType.*;

public class ScreenScale {
	final static public RectSize slingshot = new RectSize(65, 201); // 66, 206
	final static public CircleSize[] birds = new CircleSize[10];

	static {
		birds[RedBird.id] = new CircleSize(49, 47, 17.0, 5.0, 7.0);     // 45, 44
		birds[BlueBird.id] = new CircleSize(32, 31, 12.0, 2.0, 3.0);    // 30, 29
		birds[BlackBird.id] = new CircleSize(67, 89, 20.0, 3.0, 15.0);  // 64, 86
		birds[YellowBird.id] = new CircleSize(67, 54, 16.0, 7.0, 8.0);  // 56, 53
		birds[WhiteBird.id] = new CircleSize(86, 106, 26.0, 9.0, 16.0); // 80, 94
	}

	static public class RectSize {
		public double width, height;
		public RectSize(double w, double h) { width = w; height = h; }
		public double ratioW7H() { return width / height; }
		public double ratioH7W() { return height / width; }
	}

	static public class CircleSize extends RectSize {
		public double radius;
		public Point2D.Double offset;
		public CircleSize(double w, double h, double r, double offX, double offY) {
			super(w, h); radius = r;
			offset = new Point2D.Double(offX, offY);
		}
	}
}
