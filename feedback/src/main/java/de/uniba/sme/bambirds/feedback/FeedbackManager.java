package de.uniba.sme.bambirds.feedback;

import java.util.ArrayList;
import java.util.List;
import java.lang.NullPointerException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import de.uniba.sme.bambirds.common.objects.Shot;

/**
 * starts the feedback and manages its results. The shots that are marked by {@link de.uniba.sme.bambirds.feedback.UnsureEvaluation} and {@link de.uniba.sme.bambirds.feedback.Evaluation} are saved in {@code failedShots}.
 * these shots are then sorted out in {@link de.uniba.sme.bambirds.feedback.ShotSelection}
 */

public class FeedbackManager {
	private static final Logger log = LogManager.getLogger(FeedbackManager.class);
	private List<FailedShot> failedShots;
	private Evaluation evaluation;
	private ShotInformationController controller;
	private UnsureEvaluation unsureEvaluation;
	private int currentPoints;
	private int lastLevel;

// 1. erzeugen
	public FeedbackManager(ShotInformationController controller){
		Evaluation evaluation = new Evaluation();
		this.evaluation = evaluation;
		unsureEvaluation = new UnsureEvaluation(this);
		List<FailedShot> failedShot = new ArrayList<FailedShot>();
		this.failedShots = failedShot;
		this.controller = controller;
		this.currentPoints = 0;
		this.lastLevel = -1;
	}


	/**
	 * Starts all the feedback of the classes {@link de.uniba.sme.bambirds.feedback.Evaluation} and {@link de.uniba.sme.bambirds.feedback.UnsureEvaluation} 
	 */
// nach Wait until Scene is stable() in ShotExcecutor
	public void startFeedback() {

		log.info("Feedback is started");
		// shotinfo erzeugt
		
		ShotInformation shotInfo = controller.evaluateShotInformation(currentPoints, lastLevel);
		if(shotInfo.getCurrentLevel().getCurrentScore()==0){
			// last shot in level, so points are not accurate. 
			// alle schweine werden also zum letzten schuss hinzugefügt, wird hier aber ignoriert, da der letzte schuss eh keine schüsse
			// mehr aus savedshots entfernen kann.
			return;
		}
		currentPoints = shotInfo.getCurrentLevel().getCurrentScore();
		lastLevel = shotInfo.getCurrentLevel().levelId;


		if (evaluation.isFailedShot(shotInfo)) {
			int level = shotInfo.getLevelID();
			Shot shot = shotInfo.getShot();

			FailedShot failedShot = new FailedShot(level, shot);
			failedShots.add(failedShot);
		}
		unsureEvaluation.evaluate(shotInfo);

	}


	public List<FailedShot> getFailedShot() {
		return failedShots;
	}

	public void setFailedShot(List<FailedShot> failedShot) {
		this.failedShots = failedShot;
	}

}
