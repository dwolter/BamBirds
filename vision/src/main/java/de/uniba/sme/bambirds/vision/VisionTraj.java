package de.uniba.sme.bambirds.vision;

import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;

import java.awt.Point;
import java.awt.Color;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.Comparator;
import java.util.List;
import java.util.LinkedList;

public class VisionTraj {
	private BufferedImage image;
	private int _nHeight; // height of the scene
	private int _nWidth; // width of the scene

	public String identifier = "lvl";

	static private VisualDebugger DBG = new VisualDebugger("VisionTraj");
	{ DBG.enableDebug(false, false); }

	/**
	 * Custom image processing to extract trajectory points, eg. <br>
	 * {@link #findTrajectory(Slingshot, Point2D.Double, List)}
	 */
	public VisionTraj(BufferedImage img) {
		try {
			image = img; // ActionRobot.get().screenshotWithoutUI(0);
			_nHeight = image.getHeight();
			_nWidth = image.getWidth();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Get the trajectory points by image processing; the remaining tiny clouds.
	 * @param tapPoint Ignore all trajectory points after bird tap (pass {@code null} to ignore)
	 * @param traj An empty {@code List<Point2D.Double>}
	 * @return Actual tap point, Trajectory points are returned in parameter traj
	 */
	public Point2D.Double findTrajectory(final Slingshot sling, final Point2D.Double tapPoint, List<Point2D.Double> traj)
	{
		if (sling == null || traj == null)
			return new Point2D.Double();

		final double[] tapCutoff = { Double.MAX_VALUE, Double.NaN, Double.NaN }; // dist, x, y
		Rectangle crop = new Rectangle(0, 0, _nWidth, _nHeight);

		VisionHelper.loopFillObjects(image, crop, new VisionHelper.FillProcessing() {
			@Override public boolean threshold(int x, int y, int r, int g, int b) {
				if (x < sling.getX() + sling.getWidth()) // ignore points left of sling
					return false;
				return (r == g && r == b && r >= 242);
			}
			@Override public void result(Rectangle rect, LinkedList<Point> pointCloud) {
				// only blobs with max 4x4
				if (rect.width < 4 && rect.height < 4) {
					Point2D.Double avgPoint = new Point2D.Double(0, 0);
					for (Point pt : pointCloud) {
						avgPoint.x += pt.x;
						avgPoint.y += pt.y;
					}
					avgPoint.x /= pointCloud.size();
					avgPoint.y /= pointCloud.size();
					traj.add(avgPoint);
				}
				else if (tapPoint != null) {
					double dist = tapPoint.distance(rect.getLocation());
					if (dist < tapCutoff[0]) {
						tapCutoff[0] = dist;
						tapCutoff[1] = rect.getCenterX();
						tapCutoff[2] = rect.getCenterY();
					}
				}
			}
		});
		traj.sort(Comparator.comparing(rect -> rect.x));

		//TODO: move this to a better location
		DBG.setBlack();
		DBG.drawPoints(traj, 0xFF0000); // red means deleted points
		if (tapPoint != null)
			DBG.drawTarget(tapPoint, Color.ORANGE); // calculated tap point

		if (Double.isNaN(tapCutoff[1]) || Double.isNaN(tapCutoff[2]))
			return null;
		return new Point2D.Double(tapCutoff[1], tapCutoff[2]);
	}

	/**
	 * Takes raw trajectory points and filters for actual points, removing outliers.
	 * @param traj The raw points
	 * @param actualTapPoint Tap point returned by {@link #findTrajectory(Slingshot, Point2D.Double, List)}
	 * @param origin Usually the slingshot pivot. Or Actual Tap Point for after tap analysis
	 * @param originAngle LaunchAngle for Slingshot and Impact Angle for after tap analysis
	 * @param slingWidth Scale independent distance between trajectory points
	 * @param ignoreBefore All points before the tap are ignored
	 * @param ignoreAfter All points after the tap are ignored
	 * @return Filtered List
	 */
	public List<Point2D.Double> filteredTrajectory(List<Point2D.Double> traj, Point2D.Double actualTapPoint,
			Point2D.Double origin, double originAngle, double slingWidth, boolean ignoreBefore, boolean ignoreAfter)
	{
		if (traj == null || traj.isEmpty() || origin == null)
			return traj;

		LinkedList<Point2D.Double> filteredList = new LinkedList<>();
		Point2D.Double prevPoint = origin;
		double prevAngle = originAngle;

		for (Point2D.Double p : traj) {
			if (ignoreBefore && actualTapPoint != null && p.x < actualTapPoint.x)
				continue;
			else if (ignoreAfter && actualTapPoint != null && p.x > actualTapPoint.x - 15) // less cuz blue split
				break; // stop trajectory at tap point, eg. for blue or yellow bird

			double dist = p.distance(prevPoint);
			if (dist < 3 * slingWidth || p.y < 40) { // also add points exceeding the top window edge
				double newAngle = Math.atan2(prevPoint.y - p.y, p.x - prevPoint.x);
				double deltaAngle = Math.abs(prevAngle - newAngle);
				if (deltaAngle < 0.4 || deltaAngle > Math.PI - 0.4) {
					prevPoint = p;
					prevAngle = newAngle;
					filteredList.add(p);
				}
			}
		}

		// TODO: move this to a better location
		DBG.drawPoints(filteredList, 0xFFFFFF); // white means remaining points
		DBG.drawPoint(origin, 0x00FFFF); // sling pivot
		if (actualTapPoint != null)
			DBG.drawTarget(actualTapPoint, Color.cyan); // actual tap point
		DBG.saveToFile(String.format("%s_parabola", identifier));

		return filteredList;
	}
}
