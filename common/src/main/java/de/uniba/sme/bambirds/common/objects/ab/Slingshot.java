package de.uniba.sme.bambirds.common.objects.ab;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;

import static de.uniba.sme.bambirds.common.utils.Settings.USE_NEW_SLING_DETECTION;

public class Slingshot extends ABObject {
	/**
	 *
	 */
	private static final long serialVersionUID = -7335214223585382801L;

	private double _sceneScale = 1.0;
	public Point2D.Double pivot;

	public Slingshot(Rectangle bounds) {
		this(bounds, null);
	}

	public Slingshot(Rectangle bounds, Point2D.Double pivot) {
		super(bounds, ABType.Sling);
		_sceneScale = getWidth() + getHeight();
		if (pivot == null)
			pivot = calculatePivot();
		this.pivot = pivot;
	}

	public double getSceneScale() { return _sceneScale; }
	public Point getPivotPoint() { return new Point((int)pivot.x, (int)pivot.y); }

	private Point2D.Double calculatePivot() {
		if (USE_NEW_SLING_DETECTION)
			return new Point2D.Double(getX() + 0.56 * getWidth(), getY() + 0.54 * getWidth()); // new detection
		else
			return new Point2D.Double(getX() + 0.562 * getWidth(), getY() + 0.665 * getWidth()); // original detection
//		return new Point2D.Double(bounds.x + 0.564 * bounds.width, bounds.y + 0.167 * bounds.height);
	}

	@Override
	public void draw(Graphics2D g, boolean fill, Color color, double padding) {
		super.draw(g, fill, color, padding);
		g.setColor(color);
		g.drawOval(round(pivot.getX()-2), round(pivot.getY()-2), 4, 4);
	}

	@Override 
	public String toString() {
		return String.format("(Slingshot [%.1f, %.1f, %.1f, %.1f] pivot [%.3f, %.3f] scale: %.1f)",
				getX(), getY(), getWidth(), getHeight(), pivot.x, pivot.y, _sceneScale);
	}
}
