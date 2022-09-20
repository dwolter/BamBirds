package de.uniba.sme.bambirds.common.objects.ab;

import de.uniba.sme.bambirds.common.utils.MathUtil;

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

	private final double sceneScale;
	private final Point2D.Double pivot;
	private final Rectangle bounds;

	public Slingshot(final Rectangle bounds) {
		this(bounds, null);
	}

	public Slingshot(final Rectangle bounds, final Point2D.Double pivot) {
		super(bounds, ABType.Sling);
		setGlobalID("sling");
		sceneScale = getWidth() + getHeight();
		this.pivot = pivot == null ? calculatePivot() : pivot;
		this.bounds = bounds;
	}

	public double getSceneScale() {
		return sceneScale;
	}

	public Rectangle getBounds() {
		return bounds;
	}

	public Point getPivotPoint() {
		return new Point(MathUtil.round(getPivot().x), MathUtil.round(getPivot().y));
	}

	public Point2D.Double getPivotPoint2D() {
		return getPivot();
	}

	private Point2D.Double calculatePivot() {
		if (USE_NEW_SLING_DETECTION) {
			return new Point2D.Double(getX() + 0.56 * getWidth(), getY() + 0.54 * getWidth()); // new detection
		} else {
			return new Point2D.Double(getX() + 0.562 * getWidth(), getY() + 0.665 * getWidth()); // original detection
		}
//		return new Point2D.Double(bounds.x + 0.564 * bounds.width, bounds.y + 0.167 * bounds.height);
	}

	@Override
	public void draw(final Graphics2D g, final boolean fill, final Color color, final double padding) {
		super.draw(g, fill, color, padding);
		g.setColor(color);
		g.drawOval(MathUtil.round(getPivot().getX() - 2), MathUtil.round(getPivot().getY() - 2), 4, 4);
	}

	@Override
	public String toString() {
		return String.format("(Slingshot [%.1f, %.1f, %.1f, %.1f] pivot [%.3f, %.3f] scale: %.1f)",
				getX(), getY(), getWidth(), getHeight(), getPivot().x, getPivot().y, sceneScale);
	}

	public Point2D.Double getPivot() {
		return pivot;
	}
}
