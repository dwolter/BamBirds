package de.uniba.sme.bambirds.vision;

import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.database.AbstractScene;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import javax.imageio.ImageIO;

import java.awt.image.BufferedImage;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertFalse;

public class SceneTest {

	@ParameterizedTest
	@ValueSource(strings = {"/images/sb-level0.png", "/images/sb-level1.png"})
	public void JSONTest(String imageFile) throws IOException, SceneInitialisationException {
		BufferedImage img = ImageIO.read(getClass().getResource(imageFile));

		AbstractScene scene = new Scene(img);

		String json = scene.toJSON();

		System.out.println(json);

		assertFalse(json.isEmpty(), "json was expected to not be empty");

	}

}