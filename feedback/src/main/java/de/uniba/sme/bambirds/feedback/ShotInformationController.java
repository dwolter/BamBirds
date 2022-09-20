package de.uniba.sme.bambirds.feedback;


import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.geom.Point2D;

/**
 * Class to collect all data used for the feedback evaluation of the shots by being saved and called troughout the resto of the agent.
 * After the shot was executed and all data is collected it creates an instance of {@link de.uniba.sme.bambirds.feedback.ShotInformation}
 *
 * @author Hannes Altmann
 */

public class ShotInformationController {

	private static final Logger log = LogManager.getLogger(ShotInformationController.class);

	private SequenceCalculator mapper;
	private ABType birdType;

	private Shot shot;
	private Level currentLevel;
	private SequenceCalculator calculator;

	//INFORMATION PLANNED PARABOLA
	private boolean isDemoShot;
	private Plan chosenTarget;

	private double shotAngle;


	//INFORMATION ACTUAL PARABOLA
	private double actualAngle;
	private Point2D.Double actualTapPoint;
	private double actualVelocity;
	private Point2D.Double origin;
	private int removedShotsCounter = 0;
	private AbstractScene sceneBeforeShot;

	public void setSceneBeforeShot(AbstractScene sceneBeforeShot) {
		this.sceneBeforeShot = sceneBeforeShot;
	}

	public void setRemovedShotsCounter(int removedShotsCounter) {
		this.removedShotsCounter = removedShotsCounter;
	}


	public void setLevelInformation(Level level) { //FROM: ShotExecutor (constructor)
		this.currentLevel = level;
	}

	public void setShot(Shot shot) { //FROM: ShotExecutor
		this.shot = shot;
	}

	public void setChosenTarget(Plan chosenTarget) { //FROM: ShotSelection
		this.chosenTarget = chosenTarget;
	}

	public void setActualTapPoint(Point2D.Double actualTapPoint) { //FROM: VisionHelper
		this.actualTapPoint = actualTapPoint;
	}

	public void setShotAngle(double theta) {
		this.shotAngle = theta;
	}

	public void setActualVelocity(double velocity) { //FROM: ShotParabola (über VisionHelper)
		this.actualVelocity = velocity;
	}

	public void setActualOrigin(Point2D.Double origin) {//FROM: VisionHelper
		this.origin = origin;
	}

	public void setCalculator(SequenceCalculator calculator) {
		this.calculator = calculator;
	}

	public void setDemoShot(boolean isDemoShot) { //FROM: ShotSelection
		this.isDemoShot = isDemoShot;
	}

	//public void setActualAngle(){
	//TODO: hier nicht realisiert, da nicht benötigt und passt nicht zum projektaufbau, aber zu finden in: ShotParabola->launchAngle
	//}

	/**
	 * Initializes an instance {@link de.uniba.sme.bambirds.feedback.ShotInformation} with the information collected until now.
	 *
	 * @param currentPoints The current points for this level
	 * @param lastLevel     The last level executed before the current (-1 if none)
	 */
	public ShotInformation evaluateShotInformation(int currentPoints, int lastLevel) {
		int pointsGained = currentLevel.currentScore - currentPoints;
		if (lastLevel == -1 || lastLevel != currentLevel.levelId) {
			pointsGained = currentLevel.currentScore;
		}
		this.birdType = currentLevel.currentScene.getBirdTypeOnSling();
		ShotInformation shotInfo = new ShotInformation(pointsGained, calculator, shot, currentLevel, isDemoShot, chosenTarget, shotAngle, actualAngle, actualTapPoint, actualVelocity, origin, birdType, removedShotsCounter, sceneBeforeShot);
		return shotInfo;
	}
}
