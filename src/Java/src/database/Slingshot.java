package database;

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;

import static helper.Constants.USE_NEW_SLING_DETECTION;

public class Slingshot {
	private double _sceneScale = 1.0;
	public Point2D.Double pivot;
	public Rectangle bounds;

	public Slingshot(Rectangle bounds, Point2D.Double pivot) {
		this.pivot = pivot;
		updateBounds(bounds);
		if (pivot == null)
			this.pivot = calculatePivot();
	}

	public void updateBounds(Rectangle bounds) {
		this.bounds = bounds;
		_sceneScale = bounds.width + bounds.height;
	}

	public double getSceneScale() { return _sceneScale; }
	public Point getPivotPoint() { return new Point((int)pivot.x, (int)pivot.y); }

	private Point2D.Double calculatePivot() {
		if (USE_NEW_SLING_DETECTION)
			return new Point2D.Double(bounds.x + 0.56 * bounds.width, bounds.y + 0.54 * bounds.width); // new detection
		else
			return new Point2D.Double(bounds.x + 0.562 * bounds.width, bounds.y + 0.665 * bounds.width); // original detection
//		return new Point2D.Double(bounds.x + 0.564 * bounds.width, bounds.y + 0.167 * bounds.height);
	}

	@Override public String toString() {
		return String.format("(Slingshot [%d, %d, %d, %d] pivot [%.3f, %.3f] scale: %.1f)",
				bounds.x, bounds.y, bounds.width, bounds.height, pivot.x, pivot.y, _sceneScale);
	}
}
