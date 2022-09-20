package de.uniba.sme.bambirds.feedback;

import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.database.AbstractScene;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * This class works contrary to {@link de.uniba.sme.bambirds.feedback.Evaluation} to evaluate a shot from the agent. While the other class evaluates the shots based on really hard facts
 * this class uses different less exact criterias of Evaluation and compares them in the class {@link de.uniba.sme.bambirds.feedback.CriteriaComparator}
 * The use of the criterias of this class can be deactivated by setting {@code useUnsurecriterias} to false. In this case the data of the shot will still be saved for reference in {@link de.uniba.sme.bambirds.feedback.CriteriaComparator}
 *
 *@author Hannes Altmann
 */

public class UnsureEvaluation{

	private static final Logger log = LogManager.getLogger(UnsureEvaluation.class);

  private boolean useUnsureCriterias = true;
  
  
  private ShotInformation shotInformation;
  private FeedbackManager feedbackManager;
  private CriteriaComparator comparator;
  List<Integer> pointsPerShotThisLevel;
  private List<EvaluationCriteria> appliedCriterias;
  private List<Integer> numberOfObjectsMovedThisLevel;

  private int currentLevel;
  private List<Integer> pointsForThisLevel;


  public UnsureEvaluation(FeedbackManager feedbackManager){
		this.feedbackManager = feedbackManager;
		this.comparator = new CriteriaComparator(useUnsureCriterias);
		this.pointsPerShotThisLevel = new ArrayList<Integer>();
		this.appliedCriterias = new ArrayList<EvaluationCriteria>();
		this.numberOfObjectsMovedThisLevel = new ArrayList<Integer>();
		this.currentLevel = -1;
  }

	public List<EvaluationCriteria> getAppliedCriterias(){
		return appliedCriterias;
	}


	/**
	 * Evaluates a shot by using {@link de.uniba.sme.bambirds.feedback.ShotInformation} and compares the use of the saved criterias
	 * @param shotInformation the data of the shot to evaluate
	 */
	public void evaluate(ShotInformation shotInformation){
		this.shotInformation = shotInformation;

		if(currentLevel==-1){
			currentLevel = shotInformation.getLevelID();
		}

		if(currentLevel!=shotInformation.getLevelID()){
			numberOfObjectsMovedThisLevel = new ArrayList<Integer>();
			pointsPerShotThisLevel= new ArrayList<Integer>();
			currentLevel=shotInformation.getLevelID();
		}
		appliedCriterias = new ArrayList<EvaluationCriteria>();
		pointsPerShotThisLevel.add(shotInformation.getPointsGained());
		numberOfObjectsMovedThisLevel.add(shotInformation.getObjectsMoved().size());
		//apply criterias
		pointDistributionNotGood();
		pointsGainedTooLow();
		pigsHitButLowPoints();
		notMuchMovedAndNoPigsDestroyed();
		notMuchMovedAndPigsDestroyed();
		objectsMovedDistributionNotGood();
		nearMissLikely();

		if(appliedCriterias.size()>0){
			log.info(appliedCriterias.size() + " unsure criterias have been found");
		}
		log.info("Starting the evaluation of the criterias");
		comparator.compareNewInvokedCriterias(appliedCriterias, shotInformation.getPointsGained(), shotInformation.getLevelID(), feedbackManager.getFailedShot().size(), shotInformation.getRemovedShotsCounter());


		if((!appliedCriterias.isEmpty())&& useUnsureCriterias){
			List<FailedShot> failedShots = feedbackManager.getFailedShot();
			FailedShot newFailedShot = new FailedShot(shotInformation.getLevelID(), shotInformation.getShot());
			if(!failedShots.contains(newFailedShot)){
				failedShots.add(new FailedShot(shotInformation.getLevelID(), shotInformation.getShot()));
			}
		}
	}

  private void pointsGainedTooLow(){
    if(shotInformation.getPointsGained()<1000&&shotInformation.getMurderedPigs().isEmpty()){
      appliedCriterias.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
    }
  }

  private void pigsHitButLowPoints(){
	  if(shotInformation.getPointsGained()<1000&&(!shotInformation.getMurderedPigs().isEmpty())){
		  appliedCriterias.add(EvaluationCriteria.PIGSHITBUTNOTENOUGHPOINTS);
	  }
  }

  private void pointDistributionNotGood(){ // way less points than level average and no pig
	if(shotInformation.getMurderedPigs().isEmpty()){
		int sum = 0;
		int average;
		for(int points : pointsPerShotThisLevel){
			sum = sum + points;
		}
		average = sum / pointsPerShotThisLevel.size();

		if(shotInformation.getPointsGained()<average -2000){
			appliedCriterias.add(EvaluationCriteria.POINTDISTRIBUTIONNOTGOOD);
		}
	}
  }

  private void notMuchMovedAndNoPigsDestroyed(){ //add Destroyed

	if(!shotInformation.getMurderedPigs().isEmpty()){
		return;
	}

	  List<SequenceObject> movedObjects = shotInformation.getObjectsMoved();
	  double sum = 0;
	  for(SequenceObject object : movedObjects){
		sum = sum + euclidianDistanceToOldPosition(object);
	  }
	  if(sum<100){
		  appliedCriterias.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
	  }
  }

  private void notMuchMovedAndPigsDestroyed(){
	  if(shotInformation.getMurderedPigs().isEmpty()){
		return;
	}

	  List<SequenceObject> movedObjects = shotInformation.getObjectsMoved();
	  double sum = 0;
	  for(SequenceObject object : movedObjects){
		sum = sum + euclidianDistanceToOldPosition(object);
	  }
	  if(sum<20){
		  appliedCriterias.add(EvaluationCriteria.NOTMUCHMOVEDBUTPIGS);
	  }
  }

	private void objectsMovedDistributionNotGood(){
		//only if no pigs hit
		if(shotInformation.getMurderedPigs().isEmpty()){
			int sum = 0;
			int average;

			for(Integer objectsMovedNumber : numberOfObjectsMovedThisLevel){
				sum = sum + objectsMovedNumber;
			}


			average = sum / pointsPerShotThisLevel.size();

			if(shotInformation.getPointsGained()<average - 50){
				appliedCriterias.add(EvaluationCriteria.OBJECTSMOVEDDISTRIBUTIONNOTGOOD);
			}
		}
	}
  private void nearMissLikely(){
	  Plan chosenTarget = shotInformation.getChosenTarget();
	  if(chosenTarget==null){
		  return;
	  }
	  AbstractScene scene = shotInformation.getSceneBeforeShot();
	  ABObject objectFromTarget = scene.findObjectWithID(chosenTarget.getTargetObject());
	  if(objectFromTarget!=null){
		  List<SequenceObject> unmovedObjects = shotInformation.getUnmovedObjects();
		  for(SequenceObject unmovedObject : unmovedObjects){
			  ABObject first = unmovedObject.getObjectList().get(0);
			  if(isSameABObject(objectFromTarget, first)){
				  appliedCriterias.add(EvaluationCriteria.NEARMISSLIKELY);
				  return;
			  }
		  }
	  } else {
		  log.info("Finding near miss: target Object could not be found in abstract");
	  }

  }

	private boolean isSameABObject(ABObject object1, ABObject object2){
		if(getAbsolute(object1.getCenterX() - object2.getCenterX()) > 5){
			return false;
		}
		
		if(getAbsolute(object1.getCenterY() - object2.getCenterY()) > 5){
			return false;
		}
		
		if(getAbsolute(object1.getAngle() - object2.getAngle()) > 5){
			return false;
		}
		
		if(getAbsolute(object1.getHeight() - object2.getHeight()) > 5){
			return false;
		}
		if(getAbsolute(object1.getWidth() - object2.getWidth()) > 5){
			return false;
		}
		
		if(object1.getType().equals(object2.getType())){
			return true;
		}
		
		return false;
		
	}

	private double getAbsolute(double value){
		if(value<0){
			return value*-1;
		} else {
			return value;
		}
	}

  private double euclidianDistanceToOldPosition(SequenceObject object){
	  //calculates distance between centers
	  ABObject first = object.getObjectList().get(0);
	  Point firstCenter = first.getCenter();
	  Point lastCenter = object.getLastObject().getCenter();
	  double xDistance = firstCenter.getX()- lastCenter.getX();
	  double yDistance = firstCenter.getY()- lastCenter.getY();
	  return Math.sqrt((xDistance*xDistance)+(yDistance*yDistance));
  }
}
