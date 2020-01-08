package shot;

import features.VisualDebugger;
import helper.ParabolaMath;

import java.awt.Color;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.List;

public class ShotParabola {
	static private VisualDebugger DBG = new VisualDebugger("ShotParabola");
	{ DBG.enableDebug(false, true); }

	public double[] unnormalized = new double[] {0,0,0};
	public double[] normalized = new double[] {0,0,0};
	public double actualAngle;
	public double launchAngle = 0;
	public double velocity = 0;

	public Point2D.Double origin;
	public double sceneScale;

	public ShotParabola(double theta, Point2D.Double origin, double sceneScale, List<Point2D.Double> points) {
		this.actualAngle = theta;
		this.origin = origin;
		this.sceneScale = sceneScale;
		if (points == null || points.isEmpty()) return;

		unnormalized = ShotHelper.parabolaFromPointCloud(points);
		if (unnormalized[0] == 0) return;

		normalized = ParabolaMath.parabolaWithNewOrigin(unnormalized, origin.x);
		normalized[0] *= -1;
		normalized[1] *= -1;

		// Alternative method
//		double[] normalized2 = ShotHelper.solveQuadratic(points, sling);
//		double v = ShotHelper.parabolaToVelocity(normalized2, 1);

		// Calculate actual velocity for parabola and update scaling factor
		double[] v = ParabolaMath.parabolaToVelocityComponents(normalized, sceneScale);
		velocity = ParabolaMath.velocityComponentsToVelocity(v);
		launchAngle = ParabolaMath.velocityComponentsToAngle(v);
	}

	public void draw(BufferedImage img, Point2D.Double estTapPoint) {
		if (DBG.canOutputImage()) {
			DBG.setImage(img);
			if (estTapPoint != null)
				DBG.drawTarget(estTapPoint, Color.BLUE);
			DBG.drawTarget(origin, Color.WHITE);
			DBG.drawQuadraticWithOffset(normalized, origin, 0xff0000); // actual parabola
			double[] w = ShotHelper.angleToParabola(actualAngle, sceneScale);
			DBG.drawQuadraticWithOffset(w, origin, 0xff00); // predicted parabola
			DBG.saveToFile("ShotParabola_" + DBG.incrementCounter());
		}
	}

	public String toCSV() {
		return String.format("%+05.1f;%+.7f;%+.7f;%+.7f;%+.7f;%+.7f;%+.7f;%+.7f;%+.7f",
				Math.toDegrees(actualAngle), actualAngle, (launchAngle - actualAngle), velocity,
				normalized[0], normalized[1], unnormalized[0], unnormalized[1], unnormalized[2]);
	}

	@Override public String toString() {
		return String.format("Parabola @%.1f° rad: %.7f (%.7f) v: %.7f :: %.7fx^2%+.7fx :: %.7fx^2%+.7fx%+.7f",
				Math.toDegrees(actualAngle), actualAngle, (launchAngle - actualAngle), velocity,
				normalized[0], normalized[1], unnormalized[0], unnormalized[1], unnormalized[2]);
	}
}
