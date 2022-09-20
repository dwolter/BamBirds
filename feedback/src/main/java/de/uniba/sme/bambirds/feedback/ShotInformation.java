package de.uniba.sme.bambirds.feedback;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

/**
 * Class to store all data used for the feedback evaluation of the shots. It gets created after every shot done and concluded by the agent and makes the data easily accessible.
 *
 * @author Hannes Altmann
 */

public class ShotInformation {

	int pointsGained;
	private SequenceCalculator mapper;

	private Shot shot;
	private Level currentLevel;
	private ABType birdType;
	//INFORMATION PLANNED PARABOLA
	private boolean isDemoShot;
	private Plan chosenTarget;
	private double shotAngle; //Angle from Shot!

	//INFORMATION ACTUAL PARABOLA
	private double actualAngle;
	private Point2D.Double actualTapPoint;
	private double actualVelocity;
	private Point2D.Double origin;
	private int removedShotsCounter;
	private AbstractScene sceneBeforeShot;

	public ShotInformation(int pointsGained, SequenceCalculator mapper, Shot shot, Level currentLevel, boolean isDemoShot, Plan chosenTarget, double shotAngle, double actualAngle, Point2D.Double actualTapPoint, double actualVelocity, Point2D.Double origin, ABType birdType, int removedShotsCounter, AbstractScene sceneBeforeShot) {
		this.pointsGained = pointsGained;
		this.mapper = mapper;
		this.shot = shot;
		this.currentLevel = currentLevel;
		this.isDemoShot = isDemoShot;
		this.chosenTarget = chosenTarget;
		this.shotAngle = shotAngle;
		this.actualAngle = actualAngle;
		this.actualTapPoint = actualTapPoint;
		this.actualVelocity = actualVelocity;
		this.origin = origin;
		this.birdType = birdType;
		this.removedShotsCounter = removedShotsCounter;
		this.sceneBeforeShot = sceneBeforeShot;
	}

	/**
	 * Filters the {@code destroyedObjects} for pigs and returns them
	 *
	 * @returns murderedPigs the pigs murdered this round as found out by {@link de.uniba.sme.bambirds.feedback.SequenceCalculator}
	 */

	public List<SequenceObject> getMurderedPigs() {
		List<SequenceObject> murderedPigs = new ArrayList<SequenceObject>();
		List<SequenceObject> destroyedObjects = mapper.getDestroyedObjects();
		for (SequenceObject object : destroyedObjects) {
			ABObject lastObject = object.getLastObject();
			if (lastObject == null) {
				continue;
			}
			if (lastObject.getType() == ABType.Pig) {
				murderedPigs.add(object);
			}
		}
		return murderedPigs;
	}

	public AbstractScene getSceneBeforeShot() {
		return sceneBeforeShot;
	}

	public int getRemovedShotsCounter() {
		return removedShotsCounter;
	}


	public List<SequenceObject> getObjectsMoved() {
		return mapper.getMovedObjects();
	}

	public List<SequenceObject> getUnmovedObjects() {
		return mapper.getUnmovedObjects();
	}

	public List<SequenceObject> getObjectsDestroyed() {
		return mapper.getDestroyedObjects();
	}

	public ABType getBirdShot() {
		return birdType;
	}

	public int getPointsGained() {
		return pointsGained;
	}

	public int getLevelID() {
		return currentLevel.levelId;
	}

	public SequenceCalculator getMapper() {
		return mapper;
	}

	public Shot getShot() {
		return shot;
	}

	public Level getCurrentLevel() {
		return currentLevel;
	}

	public boolean getIsDemoShot() {
		return isDemoShot;
	}

	public Plan getChosenTarget() {
		return chosenTarget;
	}

	public double getShotAngle() {
		return shotAngle;
	}

	public double getActualAngle() {
		return actualAngle;
	}

	public Point2D.Double getActualTapPoint() {
		return actualTapPoint;
	}

	public double actualVelocity() {
		return actualVelocity;
	}

	public Point2D.Double getOrigin() {
		return origin;
	}

	public ABType getBirdType() {
		return birdType;
	}

}
