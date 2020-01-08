package tester;

import ab.vision.ABType;
import database.Slingshot;
import features.VisualDebugger;
import helper.Constants;
import helper.CustomLogger;
import helper.ParabolaMath;
import meta.ActionRobot;
import shot.ShotHelper;
import shot.ShotParabola;
import shot.ShotPlanner;
import shot.VisionTraj;

import java.awt.Color;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.List;

public class ParabolaTester {
	final static private Rectangle clickableArea = new Rectangle(222, 88, 555, 245);
	final static private double scaling = 0.998876;
	final static private Slingshot sling = new Slingshot( (Constants.USE_NEW_SLING_DETECTION ?
			new Rectangle(185, 320, 19, 62) : // using VisionSling
			new Rectangle(186,319,17,64) // using VisionMBR
		), null);
	private ABType birdType;
	static private VisualDebugger DBG = new VisualDebugger("ParabolaTeter");
	static{ DBG.enableDebug(false, false); }

	public void start() {
		CustomLogger.info("[ParabolaTester] ready.");
		this.birdType = ABType.RedBird;
		runTests();
	}

	private void loadLevel(ABType birdType)  {
		int n = 0;
		switch (birdType) {
			case BlueBird:   n = 1; break;
			case RedBird:    n = 2; break;
			case YellowBird: n = 3; break;
			case BlackBird:  n = 4; break;
			case WhiteBird:  n = 5; break;
		}
		ActionRobot.get().loadLevel((byte) n);
		ShotHelper.setProperties(scaling, birdType);
		try { Thread.sleep(100); } catch (Exception ignored) {}
	}

	/** Enrty point */
	private void runTests() {
		//shootRandomPoints(ABType.RedBird);
		//shootRandomPointsYellowTap();
		shootYellowNear();
		//createShotEvaluationForAllBirds();
		//findYellowBirdEstimation();
		/*
		findAccuratePivotPoint(ABType.BlueBird);
		findAccuratePivotPoint(ABType.RedBird);
		findAccuratePivotPoint(ABType.YellowBird);
		findAccuratePivotPoint(ABType.BlackBird);
		findAccuratePivotPoint(ABType.WhiteBird);
		*/
	}

	private void shootYellowNear() {
		this.birdType = ABType.YellowBird;
		loadLevel(birdType);
		Point2D.Double target = new Point2D.Double(474, 366);
		ShotPlanner sp = new ShotPlanner(sling, scaling, this.birdType);
		double[] release = sp.predict(target);
		shoot(release[1], 0);
	}

	// #################################################################################################
	// ####              Refine pivot point by comparing intersecting parabolas
	// #################################################################################################

	/** Perform 5 shots and triangulate subpixel pivot point (for all birds) */
	private void findAccuratePivotPoint(ABType bird) {
		this.birdType = bird;
		CustomLogger.info("Searching pivot point for " + birdType);
		loadLevel(birdType);
		Point2D.Double avgPoint = new Point2D.Double();
		double[][] allParabolas = new double[9][3];
		int i = 0;
		for (; i < 5; i++) {
			int deg = i * 10 + 20; // 20-70, stepsize 10
			ShotParabola prbl = shootAndGetParabola(Math.toRadians(deg));
			allParabolas[i] = prbl.unnormalized;
			if (i > 0) {
				Point2D.Double np = ParabolaMath.intersection(allParabolas[i - 1], allParabolas[i], sling.pivot);
				avgPoint.x += np.x;
				avgPoint.y += np.y;
				CustomLogger.info(String.format("new pivot: %1.3f %1.3f", avgPoint.x / i, avgPoint.y / i));
			}
			if (DBG.canOutputImage()) {
				DBG.doScreenshot();
				DBG.drawQuadratic(prbl.unnormalized, 0xff00);
				DBG.drawBoundingBox(sling.bounds, Color.WHITE);
				DBG.drawTarget(sling.pivot, Color.WHITE);
				DBG.saveToFile("ParabolaTester" + DBG.incrementCounter());
			}
		}
		avgPoint.x /= (i - 1);
		avgPoint.y /= (i - 1);
		CustomLogger.info(String.format("final pivot: %s", avgPoint));
		CustomLogger.info(String.format("X offset: %1.4f", (avgPoint.x - sling.bounds.x) / sling.bounds.width ));
		CustomLogger.info(String.format("Y offset: %1.4f (of width) %1.4f (of height)",
				(avgPoint.y - sling.bounds.y) / sling.bounds.width, (avgPoint.y - sling.bounds.y) / sling.bounds.height));
	}

	// #################################################################################################
	// ####            Shoot at random points and compare parabola with actual trajectory
	// #################################################################################################

	/** Shoot random points in a given area. Used for parabola accuracy evaluation */
	private void shootRandomPoints(ABType bird) {
		this.birdType = bird;
		loadLevel(birdType);
		Point2D.Double target = sling.pivot;
		//target.y -= 200;
		for (int b = 0; b < 9; b++) {
			//target.x += 100;
			target = new Point2D.Double(sling.pivot.x + 100 + Math.random() * 200, sling.pivot.y - Math.random() * 280);

			ShotPlanner sp = new ShotPlanner(sling, scaling, this.birdType);
			double[] release = sp.predict(target);
			double theta = release[1]; // 0: low shot, 1: high shot
			if (Double.isNaN(theta)) theta = release[0];
			if (Double.isNaN(theta)) continue;

			int time = sp.getTapTime(theta, target, this.birdType);
			shoot(ShotHelper.actualToLaunch(theta), time);
			Point2D.Double tap = ShotHelper.predictLocationAfterTime(theta, sling, time);

			if (DBG.canOutputImage()) {
				double[] w = sp.parabolaForActualAngle(theta);
				DBG.doScreenshot();
				DBG.drawQuadraticWithOffset(w, sling.pivot, 0xff00);
				DBG.drawTarget(target, Color.RED);
				if (tap != null)
					DBG.drawTarget(tap, Color.BLUE);
				DBG.drawBoundingBox(sling.bounds, Color.WHITE);
				DBG.drawTarget(sling.pivot, Color.WHITE);
				DBG.saveToFile("ParabolaTester" + DBG.incrementCounter());
			}
		}
	}

	/** Shoot random points in a given area. Used for parabola accuracy evaluation */
	private void shootRandomPointsYellowTap() {
		this.birdType = ABType.YellowBird;
		ShotHelper.setProperties(scaling, birdType);
		ActionRobot.get().loadLevel((byte) 6);
		for (int b = 0; b < 9; b++) {
			double t = 37 + Math.random() * 40;
			shootYellow(Math.toRadians(t), new Point2D.Double(750, 80 + Math.random() * 270));
		}
	}

	/** Shoot with fixed angle and calculate tap time accordingly */
	private void shootYellow(double theta, Point2D.Double target) {
		Point2D.Double tapPoint = ShotHelper.predictYellowBirdTapPoint(theta, sling, target);
		double thetaTap = ShotHelper.predictImpactAngle(theta, sling, tapPoint);
		double w[] = ParabolaMath.velocityToParabola(thetaTap, ShotHelper.fYellowVelocity(thetaTap), sling.getSceneScale());
		int taptime = ShotHelper.predictTime(theta, sling, tapPoint);
		shoot(ShotHelper.actualToLaunch(theta), taptime);

		if (DBG.canOutputImage()) {
			DBG.doScreenshot();
			DBG.drawTarget(target, Color.RED);
			DBG.drawTarget(tapPoint, Color.BLUE);
			DBG.drawQuadraticWithOffset(w, tapPoint, 0xff00);
			DBG.saveToFile("ParabolaTester" + DBG.incrementCounter());
		}
	}

	// #################################################################################################
	// ####              Shoot yellow bird with tap time and evaluate resulting parabola
	// #################################################################################################

	/** Perform all yellow bird tap shots to get a completely covered impact angle graph */
	private void findYellowBirdEstimation() {
		this.birdType = ABType.YellowBird;
		ShotHelper.setProperties(scaling, birdType);
		ActionRobot.get().loadLevel((byte) 6);
		int shot = 0;
		long preTime = System.currentTimeMillis();
		// Fixed TIME:
//		for (int d = 40; d <= 84; d += 5) { // launch angle
//			for (int t = 0; t < 16; t++) { // tap time: 1500 - 3000
//				int taptime = 1500 + t * 100;
//				if (shootYellowFixedTime(Math.toRadians(d), taptime)) {
//					shot++;
//					if (shot % 8 == 0)
//						ActionRobot.get().loadLevel((byte) 6);
//				}
//			}
//		}
		// Fixed TAP ANGLE:
		for (int i = -65; i <= 23; i += 2) { // impact angle
			for (int d = 40; d <= 80; d += 5) { // launch angle
				if (shootYellowFixedImpact(Math.toRadians(d), Math.toRadians(i))) {
					shot++; // 274 shots total ~> 38 min
					if (shot % 8 == 0)
						ActionRobot.get().loadLevel((byte) 6);
				}
			}
		}
		System.out.println(String.format("shots total: %d (took: %.2fs)", shot, (System.currentTimeMillis() - preTime) / 1000.0));
	}

	/** Perform shot with fixed impact angle, time is variable */
	private boolean shootYellowFixedImpact(double theta, double impact) {
		double w[] = ShotHelper.angleToParabola(theta, sling.getSceneScale());
		double x = ParabolaMath.tangentToX(w, impact);
		double y = w[0] * x * x + w[1] * x;
		Point2D.Double tapPoint = new Point2D.Double(sling.pivot.x + x, sling.pivot.y - y);
		int time = ShotHelper.predictTime(theta, sling, tapPoint);
		if (!clickableArea.contains(tapPoint) || time > 7000 || time < 100)
			return false;
		shootYellowWithTap(theta, time, impact, new Point2D.Double(tapPoint.x, tapPoint.y));
		return true;
	}

	/** Perform a shot with a fixed time, where tap point is calculated */
	private boolean shootYellowFixedTime(double theta, int time) {
		Point2D.Double tapPoint = ShotHelper.predictLocationAfterTime(theta, sling, time);
		if (tapPoint == null || !clickableArea.contains(tapPoint))
			return false;
		double impact = ShotHelper.predictImpactAngle(theta, sling, tapPoint);
		shootYellowWithTap(theta, time, impact, tapPoint);
		return true;
	}

	/** Perform shot and write output */
	private void shootYellowWithTap(double theta, int taptime, double impact, Point2D.Double tapPoint) {
		ShotParabola afterTap = shootAndGetYellowParabola(theta, taptime, tapPoint, impact);
		writeOutputYellowTap(theta, taptime, afterTap);

		if (DBG.canOutputImage()) {
			DBG.doScreenshot();
			if (tapPoint != null)
				DBG.drawTarget(tapPoint, Color.BLUE);
			DBG.drawQuadraticWithOffset(afterTap.normalized, tapPoint, 0xff00);
			DBG.saveToFile("ParabolaTester_Yellow_" +
					(int)Math.toDegrees(impact) + "_" + taptime + "_" + (int)Math.toDegrees(theta));
		}
	}

	/** Perform shot, wait 5s and then grab the actual ingame trajectory */
	private ShotParabola shootAndGetYellowParabola(double theta, int time, Point2D.Double tapPoint, double impact) {
		shoot(ShotHelper.actualToLaunch(theta), time);
		VisionTraj vision = new VisionTraj(ActionRobot.get().screenshotWithoutUI(0));
		List<Point2D.Double> pts = new LinkedList<>();
		Point2D.Double actualTap = vision.findTrajectory(sling, tapPoint, pts);
		pts = vision.filteredTrajectory(pts, actualTap, actualTap, impact, sling.bounds.width, true, false);
		return new ShotParabola(impact, actualTap, sling.getSceneScale(), pts);
	}

	/** Save results to csv file */
	private void writeOutputYellowTap(double theta, int taptime, ShotParabola pub) {
		System.out.println(String.format("Release: %.1f° (t:%d) -- %s", Math.toDegrees(theta), taptime, pub));
		try(FileWriter fw = new FileWriter("parabolaEval-Tap-Yellow.csv", true);
				BufferedWriter bw = new BufferedWriter(fw);
				PrintWriter out = new PrintWriter(bw)) {
			out.println(String.format("%+.7f;%4d;%s", theta, taptime, pub.toCSV()));
		} catch (IOException e) {
			System.out.println("[ParabolaTester] Error writing to file: " + e.getMessage());
		}
	}

	// #################################################################################################
	// ####                         Automated evaluation for all birds
	// #################################################################################################

	/** Try all angles for all birds, make sure you have loaded the parabola evaluation levels */
	private void createShotEvaluationForAllBirds() {
		this.birdType = ABType.RedBird;
		shootWholeRange(0, 74, 1); // only evaluate low shots for red since all are equal
		shootWholeRange(74, 86, 0.5f);
		this.birdType = ABType.BlueBird;
		shootWholeRange(74, 86, 0.5f);
		this.birdType = ABType.YellowBird;
		shootWholeRange(74, 86, 0.5f);
		this.birdType = ABType.BlackBird;
		shootWholeRange(70, 86, 0.5f);
		this.birdType = ABType.WhiteBird;
		shootWholeRange(69, 86, 0.5f);
	}

	/** Define range and stepsize, this method will do the rest */
	private void shootWholeRange(float start, float end, float stepSize) {
		for (float i = start; i < end; i += stepSize * 9) {
			int num = (int)Math.ceil((end - i) / stepSize);
			loadLevel(birdType);
			shootAngleRange(i, stepSize, Math.min(num, 9));
		}
	}

	/** Perform multiple shots, evaluate parabola and save to file */
	private void shootAngleRange(float start, float step, int numOfShots) {
		assert (numOfShots <= 9) && (numOfShots > 0);
		for (int u = 0; u < numOfShots; u++) {
			double theta = Math.toRadians(u * step + start);
			writeOutput(shootAndGetParabola(theta));
		}
	}

	/** Perform shot, wait 5s and then grab the actual ingame trajectory */
	private ShotParabola shootAndGetParabola(double theta) {
		shoot(theta, 0);
		VisionTraj vision = new VisionTraj(ActionRobot.get().screenshotWithoutUI(0));
		List<Point2D.Double> pts = new LinkedList<>();
		Point2D.Double actualTap = vision.findTrajectory(sling, null, pts);
		pts = vision.filteredTrajectory(pts, actualTap, sling.pivot, theta, sling.bounds.width, false, true);
		return new ShotParabola(theta, sling.pivot, sling.getSceneScale(), pts);
	}

	/** Helper function to send shoot command to the Action Robot */
	private void shoot(double theta, int time) {
		// Dont use angleToReleasePoint() since evaluation should not be biased with actualToLaunch() conversion
		Point rel = new Point((int)(- 1000 * Math.cos(theta)), (int)(+ 1000 * Math.sin(theta)));
		ActionRobot.get().shootFast((int)sling.pivot.x, (int)sling.pivot.y, rel.x, rel.y, 0, time, false);
		try { Thread.sleep((birdType == ABType.BlackBird ? 7000 : 5000)); } catch (Exception ignored) {}
	}

	/** Save results to csv file */
	private void writeOutput(ShotParabola pub) {
		CustomLogger.info(String.format("%s %s", birdType, pub));
		try(FileWriter fw = new FileWriter("parabolaEval-" + birdType + ".csv", true);
				BufferedWriter bw = new BufferedWriter(fw);
				PrintWriter out = new PrintWriter(bw)) {
			out.println(pub.toCSV());
		} catch (IOException e) {
			CustomLogger.severe("[ParabolaTester] Error writing to file: " + e.getMessage());
		}
	}
}
