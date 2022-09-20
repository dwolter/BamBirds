package de.uniba.sme.bambirds.feedback;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

public class UnsureEvaluationTest{



	@Test
	public void evaluationOfNotEnoughPointsWorks(){
		ShotInformationController controller = new ShotInformationController();
		FeedbackManager manager = new FeedbackManager(controller);
		UnsureEvaluation evaluation = new UnsureEvaluation(manager);
		ShotInformation shotInfo = mock(ShotInformation.class);
		when(shotInfo.getLevelID()).thenReturn(1);
		when(shotInfo.getShotAngle()).thenReturn(1.0);
		when(shotInfo.getPointsGained()).thenReturn(500);
		when(shotInfo.getMurderedPigs()).thenReturn(new ArrayList<SequenceObject>());
		when(shotInfo.getObjectsMoved()).thenReturn(new ArrayList<SequenceObject>());
		
		evaluation.evaluate(shotInfo);
		
		List<EvaluationCriteria> correctResult = new ArrayList<EvaluationCriteria>();
		correctResult.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		correctResult.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		assertEquals(evaluation.getAppliedCriterias(), correctResult);
	}
	
	@Test
	public void evaluationOfPigsHitButLowPointsWorks(){
		ShotInformationController controller = new ShotInformationController();
		FeedbackManager manager = new FeedbackManager(controller);
		UnsureEvaluation evaluation = new UnsureEvaluation(manager);
		ShotInformation shotInfo = mock(ShotInformation.class);
		when(shotInfo.getLevelID()).thenReturn(1);
		when(shotInfo.getShotAngle()).thenReturn(1.0);
		when(shotInfo.getPointsGained()).thenReturn(500);
		List<SequenceObject> pigsDestroyed = new ArrayList<SequenceObject>();
		SequenceObject pig = mock(SequenceObject.class);
		pigsDestroyed.add(pig);
		when(shotInfo.getMurderedPigs()).thenReturn(pigsDestroyed);
		List<SequenceObject> objectsMoved = new ArrayList<SequenceObject>();
		when(shotInfo.getObjectsMoved()).thenReturn(objectsMoved);
		
		evaluation.evaluate(shotInfo);
		
		List<EvaluationCriteria> correctResult = new ArrayList<EvaluationCriteria>();
		correctResult.add(EvaluationCriteria.PIGSHITBUTNOTENOUGHPOINTS);
		correctResult.add(EvaluationCriteria.NOTMUCHMOVEDBUTPIGS);
		assertEquals(evaluation.getAppliedCriterias(), correctResult);
	}
}
