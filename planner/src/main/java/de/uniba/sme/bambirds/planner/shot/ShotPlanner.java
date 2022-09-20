package de.uniba.sme.bambirds.planner.shot;

import java.awt.geom.Point2D;

import de.uniba.sme.bambirds.common.database.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.utils.ShotHelper;


public class ShotPlanner {
	private static final Logger log = LogManager.getLogger(ShotPlanner.class);
	
	private Slingshot _slinghot;
	private double _scalingFactor = 1.005;
	private ABType _birdType;

	public ShotPlanner(Slingshot sling, double scalingFactor, ABType birdType) {
		_slinghot = sling;
		_birdType = birdType;
		updateInternalScalingFactor(scalingFactor);
		refresh();
	}

	public ShotPlanner(Level level) {
		this(level.currentScene);
	}

	public ShotPlanner(AbstractScene scene) {
		this(scene.getSlingshot(),scene.scalingFactor, scene.getBirdTypeOnSling());
	}

	/**
	 * A bit stupid to have variables in ShotHelper, but we need to update them.
	 * In case someone else changed these, since the whole class is static.
	 */
	private void refresh() { ShotHelper.setProperties(_scalingFactor, _birdType); }

	/** Update internal scaling factor as well as {@link ShotHelper#scaleFactor} */
	private void updateInternalScalingFactor(double factor) {
		if (Double.isNaN(factor)) return;
		_scalingFactor = factor;
		refresh();
	}

	/**
	 * Calculate the two high and low shot launch points for target
	 * @return {@code double[2] = [low, high]} Shots actual angle, NaN if not available
	 */
	public double[] predict(Point2D.Double targetPoint) {
		refresh();
		return ShotHelper.estimateLaunchPoint(_slinghot, targetPoint);
	}

	/**
	 * Get tap time based on the bird type on sling
	 * @param theta Actual angle, <b>NOT</b> release point.
	 *              To convert release point use {@link ShotHelper#launchToActual(double)}
	 */
	public int getTapTime(double theta, Point2D.Double targetPoint, ABType birdType) {
		refresh();
		double interval = 0;
		switch (birdType) {
			case BlackBird: interval = 1.05; break;
			case BlueBird: interval = 0.80; break;
			case WhiteBird: interval = 0.90; break;
			case YellowBird: interval = 0.85; break;
			default: break;
		}
		return (int)(ShotHelper.predictTime(theta, _slinghot, targetPoint) * interval);
	}

	/**
	 * Calculate and return the impact angle at target (in degrees)
	 * @param theta Actual angle, <b>NOT</b> release point.
	 *              To convert release point use {@link ShotHelper#launchToActual(double)}
	 */
	public int getImpactAngle(double theta, Point2D.Double targetPoint) {
		refresh();
		double rad = ShotHelper.predictImpactAngle(theta, _slinghot, targetPoint);
		return (int)Math.toDegrees(rad);
	}

	/**
	 * @param theta Actual angle, <b>NOT</b> release point.
	 *              To convert release point use {@link ShotHelper#launchToActual(double)}
	 * @return {@code double[3]} parabola weights with {@code [a, b, 0]}
	 */
	public double[] parabolaForActualAngle(double theta) {
		refresh();
		return ShotHelper.angleToParabola(theta, _slinghot.getSceneScale());
	}

}
