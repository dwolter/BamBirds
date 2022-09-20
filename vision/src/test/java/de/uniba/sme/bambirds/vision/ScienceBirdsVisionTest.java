package de.uniba.sme.bambirds.vision;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Random;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.PythonConnector;

class ScienceBirdsVisionTest {

	@ParameterizedTest
	@ValueSource(strings = { "/sciencebirds/sb_test_image1.png" })
	@EnabledIfEnvironmentVariable(named = "BAMBIRDS_VISION_TEST", matches = "true")
	void findBirds(String sceneFile) throws IOException {
		BufferedImage img = ImageIO.read(getClass().getResource(sceneFile));
		File imgFile = ImageUtil.saveToTempFile(img, "sb_image_" + new Random().nextInt());
		PythonConnector connector = new PythonConnector("vision", imgFile.getAbsolutePath());
		Thread thread = new Thread(connector);
		thread.start();
		try {
			// Since getting a result is not yet implemented, we cannot wait for a result
			connector.getResult(0);
			thread.join();
			assertTrue((new File(imgFile.getParentFile(), imgFile.getName().replace(".png", "_watershed.png"))).exists(),
					"Watershed File should exist");
		} catch (InterruptedException e) {
			// ignored
		}
	}

}
