package de.uniba.sme.bambirds.feedback;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;
import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.objects.Shot;
import org.junit.jupiter.api.Test;

public class FeedbackManagerTest{
	
	@Test
	public void feedbackNotPigsNoPointsWorks(){
		ShotInformation shotInfo = mock(ShotInformation.class);
		when(shotInfo.getLevelID()).thenReturn(1);
		when(shotInfo.getShotAngle()).thenReturn(1.1);
		when(shotInfo.getPointsGained()).thenReturn(500);
		when(shotInfo.getMurderedPigs()).thenReturn(new ArrayList<SequenceObject>());
		when(shotInfo.getObjectsMoved()).thenReturn(new ArrayList<SequenceObject>());
		when(shotInfo.getShot()).thenReturn(new Shot());
		Level level = mock(Level.class);
		when(level.getCurrentScore()).thenReturn(500);
		when(shotInfo.getCurrentLevel()).thenReturn(level);
		ShotInformationController controller = mock(ShotInformationController.class);
		when(controller.evaluateShotInformation(0, -1)).thenReturn(shotInfo);
		
		FeedbackManager manager = new FeedbackManager(controller);
		manager.startFeedback();
		
		List<FailedShot> failedShots = new ArrayList<FailedShot>();
		failedShots.add(new FailedShot(1, new Shot()));
		
		assertEquals(failedShots, manager.getFailedShot());
	}
}