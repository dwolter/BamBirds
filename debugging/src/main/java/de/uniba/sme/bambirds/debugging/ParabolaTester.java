package de.uniba.sme.bambirds.debugging;

import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.ParabolaMath;
import de.uniba.sme.bambirds.common.utils.ShotHelper;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ShotParabola;
import de.uniba.sme.bambirds.planner.shot.ShotPlanner;
import de.uniba.sme.bambirds.vision.VisionTraj;

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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ParabolaTester {
	private static final Logger log = LogManager.getLogger();

	final static private Rectangle clickableArea = new Rectangle(222, 88, 555, 245);
	final static private double scaling = 0.998876;
	final static private Slingshot sling = new Slingshot(
			(Settings.USE_NEW_SLING_DETECTION ? new Rectangle(185, 320, 19, 62) : // using VisionSling
					new Rectangle(186, 319, 17, 64) // using VisionMBR
			), null);
	private ABType birdType;
	static private VisualDebugger DBG = new VisualDebugger("ParabolaTeter");
	static {
		DBG.enableDebug(false, false);
	}

	public void start() throws ServerException {
		log.debug("ready.");
		this.birdType = ABType.RedBird;
		runTests();
	}

	private void loadLevel(ABType birdType) throws ServerException {
		int n = 0;
		switch (birdType) {
			case BlueBird:   n = 1; break;
			case RedBird:    n = 2; break;
			case YellowBird: n = 3; break;
			case BlackBird:  n = 4; break;
			case WhiteBird:  n = 5; break;
		}
		Client.get().loadLevel(n);
		ShotHelper.setProperties(scaling, birdType);
		try { Thread.sleep(100); } catch (Exception ignored) {}
	}

	/**
	 * Enrty point
	 * 
	 * @throws ServerException
	 */
	private void runTests() throws ServerException {
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

	private void shootYellowNear() throws ServerException {
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

	/**
	 * Perform 5 shots and triangulate subpixel pivot point (for all birds)
	 * 
	 * @throws ServerException
	 */
	private void findAccuratePivotPoint(ABType bird) throws ServerException {
		this.birdType = bird;
		log.debug("Searching pivot point for " + birdType);
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
				log.debug(String.format("new pivot: %1.3f %1.3f", avgPoint.x / i, avgPoint.y / i));
			}
			if (DBG.canOutputImage()) {
				DBG.setImage(Client.get().doScreenShot());
				DBG.drawQuadratic(prbl.unnormalized, 0xff00);
				DBG.drawBoundingBox(sling, Color.WHITE);
				DBG.drawTarget(sling.pivot, Color.WHITE);
				DBG.saveToFile("ParabolaTester" + DBG.incrementCounter());
			}
		}
		avgPoint.x /= (i - 1);
		avgPoint.y /= (i - 1);
		log.debug(String.format("final pivot: %s", avgPoint));
		log.debug(String.format("X offset: %1.4f", (avgPoint.x - sling.x) / sling.width ));
		log.debug(String.format("Y offset: %1.4f (of width) %1.4f (of height)",
				(avgPoint.y - sling.y) / sling.width, (avgPoint.y - sling.y) / sling.height));
	}

	// #################################################################################################
	// ####            Shoot at random points and compare parabola with actual trajectory
	// #################################################################################################

	/**
	 * Shoot random points in a given area. Used for parabola accuracy evaluation
	 * 
	 * @throws ServerException
	 */
	private void shootRandomPoints(ABType bird) throws ServerException {
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
				DBG.setImage(Client.get().doScreenShot());
				DBG.drawQuadraticWithOffset(w, sling.pivot, 0xff00);
				DBG.drawTarget(target, Color.RED);
				if (tap != null)
					DBG.drawTarget(tap, Color.BLUE);
				DBG.drawBoundingBox(sling, Color.WHITE);
				DBG.drawTarget(sling.pivot, Color.WHITE);
				DBG.saveToFile("ParabolaTester" + DBG.incrementCounter());
			}
		}
	}

	/**
	 * Shoot random points in a given area. Used for parabola accuracy evaluation
	 * 
	 * @throws ServerException
	 */
	private void shootRandomPointsYellowTap() throws ServerException {
		this.birdType = ABType.YellowBird;
		ShotHelper.setProperties(scaling, birdType);
		Client.get().loadLevel((byte) 6);
		for (int b = 0; b < 9; b++) {
			double t = 37 + Math.random() * 40;
			shootYellow(Math.toRadians(t), new Point2D.Double(750, 80 + Math.random() * 270));
		}
	}

	/**
	 * Shoot with fixed angle and calculate tap time accordingly
	 * 
	 * @throws ServerException
	 */
	private void shootYellow(double theta, Point2D.Double target) throws ServerException {
		Point2D.Double tapPoint = ShotHelper.predictYellowBirdTapPoint(theta, sling, target);
		double thetaTap = ShotHelper.predictImpactAngle(theta, sling, tapPoint);
		double w[] = ParabolaMath.velocityToParabola(thetaTap, ShotHelper.fYellowVelocity(thetaTap), sling.getSceneScale());
		int taptime = ShotHelper.predictTime(theta, sling, tapPoint);
		shoot(ShotHelper.actualToLaunch(theta), taptime);

		if (DBG.canOutputImage()) {
			DBG.setImage(Client.get().doScreenShot());
			DBG.drawTarget(target, Color.RED);
			DBG.drawTarget(tapPoint, Color.BLUE);
			DBG.drawQuadraticWithOffset(w, tapPoint, 0xff00);
			DBG.saveToFile("ParabolaTester" + DBG.incrementCounter());
		}
	}

	// #################################################################################################
	// ####              Shoot yellow bird with tap time and evaluate resulting parabola
	// #################################################################################################

	/**
	 * Perform all yellow bird tap shots to get a completely covered impact angle
	 * graph
	 * 
	 * @throws ServerException
	 */
	private void findYellowBirdEstimation() throws ServerException {
		this.birdType = ABType.YellowBird;
		ShotHelper.setProperties(scaling, birdType);
		Client.get().loadLevel((byte) 6);
		int shot = 0;
		long preTime = System.currentTimeMillis();
		// Fixed TIME:
//		for (int d = 40; d <= 84; d += 5) { // launch angle
//			for (int t = 0; t < 16; t++) { // tap time: 1500 - 3000
//				int taptime = 1500 + t * 100;
//				if (shootYellowFixedTime(Math.toRadians(d), taptime)) {
//					shot++;
//					if (shot % 8 == 0)
//						Client.get().loadLevel((byte) 6);
//				}
//			}
//		}
		// Fixed TAP ANGLE:
		for (int i = -65; i <= 23; i += 2) { // impact angle
			for (int d = 40; d <= 80; d += 5) { // launch angle
				if (shootYellowFixedImpact(Math.toRadians(d), Math.toRadians(i))) {
					shot++; // 274 shots total ~> 38 min
					if (shot % 8 == 0)
						Client.get().loadLevel((byte) 6);
				}
			}
		}
		System.out.println(String.format("shots total: %d (took: %.2fs)", shot, (System.currentTimeMillis() - preTime) / 1000.0));
	}

	/**
	 * Perform shot with fixed impact angle, time is variable
	 * 
	 * @throws ServerException
	 */
	private boolean shootYellowFixedImpact(double theta, double impact) throws ServerException {
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

	/**
	 * Perform a shot with a fixed time, where tap point is calculated
	 * 
	 * @throws ServerException
	 */
	private boolean shootYellowFixedTime(double theta, int time) throws ServerException {
		Point2D.Double tapPoint = ShotHelper.predictLocationAfterTime(theta, sling, time);
		if (tapPoint == null || !clickableArea.contains(tapPoint))
			return false;
		double impact = ShotHelper.predictImpactAngle(theta, sling, tapPoint);
		shootYellowWithTap(theta, time, impact, tapPoint);
		return true;
	}

	/**
	 * Perform shot and write output
	 * 
	 * @throws ServerException
	 */
	private void shootYellowWithTap(double theta, int taptime, double impact, Point2D.Double tapPoint)
			throws ServerException {
		ShotParabola afterTap = shootAndGetYellowParabola(theta, taptime, tapPoint, impact);
		writeOutputYellowTap(theta, taptime, afterTap);

		if (DBG.canOutputImage()) {
			DBG.setImage(Client.get().doScreenShot());
			if (tapPoint != null)
				DBG.drawTarget(tapPoint, Color.BLUE);
			DBG.drawQuadraticWithOffset(afterTap.normalized, tapPoint, 0xff00);
			DBG.saveToFile("ParabolaTester_Yellow_" +
					(int)Math.toDegrees(impact) + "_" + taptime + "_" + (int)Math.toDegrees(theta));
		}
	}

	/**
	 * Perform shot, wait 5s and then grab the actual ingame trajectory
	 * 
	 * @throws ServerException
	 */
	private ShotParabola shootAndGetYellowParabola(double theta, int time, Point2D.Double tapPoint, double impact)
			throws ServerException {
		shoot(ShotHelper.actualToLaunch(theta), time);
		VisionTraj vision = new VisionTraj(ImageUtil.removeABUI(Client.get().doScreenShot(), 0));
		List<Point2D.Double> pts = new LinkedList<>();
		Point2D.Double actualTap = vision.findTrajectory(sling, tapPoint, pts);
		pts = vision.filteredTrajectory(pts, actualTap, actualTap, impact, sling.width, true, false);
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

	/**
	 * Try all angles for all birds, make sure you have loaded the parabola
	 * evaluation levels
	 * 
	 * @throws ServerException
	 */
	private void createShotEvaluationForAllBirds() throws ServerException {
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

	/**
	 * Define range and stepsize, this method will do the rest
	 * 
	 * @throws ServerException
	 */
	private void shootWholeRange(float start, float end, float stepSize) throws ServerException {
		for (float i = start; i < end; i += stepSize * 9) {
			int num = (int)Math.ceil((end - i) / stepSize);
			loadLevel(birdType);
			shootAngleRange(i, stepSize, Math.min(num, 9));
		}
	}

	/**
	 * Perform multiple shots, evaluate parabola and save to file
	 * 
	 * @throws ServerException
	 */
	private void shootAngleRange(float start, float step, int numOfShots) throws ServerException {
		assert (numOfShots <= 9) && (numOfShots > 0);
		for (int u = 0; u < numOfShots; u++) {
			double theta = Math.toRadians(u * step + start);
			writeOutput(shootAndGetParabola(theta));
		}
	}

	/**
	 * Perform shot, wait 5s and then grab the actual ingame trajectory
	 * 
	 * @throws ServerException
	 */
	private ShotParabola shootAndGetParabola(double theta) throws ServerException {
		shoot(theta, 0);
		VisionTraj vision = new VisionTraj(ImageUtil.removeABUI(Client.get().doScreenShot(), 0));
		List<Point2D.Double> pts = new LinkedList<>();
		Point2D.Double actualTap = vision.findTrajectory(sling, null, pts);
		pts = vision.filteredTrajectory(pts, actualTap, sling.pivot, theta, sling.width, false, true);
		return new ShotParabola(theta, sling.pivot, sling.getSceneScale(), pts);
	}

	/**
	 * Helper function to send shoot command to the Action Robot
	 * 
	 * @throws ServerException
	 */
	private void shoot(double theta, int time) throws ServerException {
		// Dont use angleToReleasePoint() since evaluation should not be biased with actualToLaunch() conversion
		Point rel = new Point((int)(- 1000 * Math.cos(theta)), (int)(+ 1000 * Math.sin(theta)));
		Shot shot = new Shot((int)sling.pivot.x, (int)sling.pivot.y, rel.x, rel.y,0, 0, 0, time);
		Client.get().shootFast(shot);
		try { Thread.sleep((birdType == ABType.BlackBird ? 7000 : 5000)); } catch (Exception ignored) {}
	}

	/** Save results to csv file */
	private void writeOutput(ShotParabola pub) {
		log.debug(String.format("%s %s", birdType, pub));
		try(FileWriter fw = new FileWriter("parabolaEval-" + birdType + ".csv", true);
				BufferedWriter bw = new BufferedWriter(fw);
				PrintWriter out = new PrintWriter(bw)) {
			out.println(pub.toCSV());
		} catch (IOException e) {
			log.error("[ParabolaTester] Error writing to file: " + e.getMessage());
		}
	}
}
