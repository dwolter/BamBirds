package de.uniba.sme.bambirds.planner.shot;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.vision.Scene;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import javax.imageio.ImageIO;
import java.io.IOException;

public class SceneDifferenceTest {
	@Test
	public void simpleIntegrationDifference() throws IOException, SceneInitialisationException {
		AbstractScene firstScene = new Scene(ImageIO.read(getClass().getResource("/scenes/difference/scene1-1.png")));
		AbstractScene secondScene = new Scene(ImageIO.read(getClass().getResource("/scenes/difference/scene1-2.png")));
		SceneDifference difference = new SceneDifference(firstScene, secondScene);
		Assertions.assertEquals(3,difference.getDestroyedObjects().size(), "Should have the same amount of destroyed objects");
		Assertions.assertEquals(9,difference.getMovedObjects().size(), "Should have the same amount of moved objects");
		Assertions.assertEquals(1,difference.getUnchangedObjects().size(), "Should have the same amount of unmoved objects");
	}
}
