package de.uniba.sme.bambirds;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.objects.AbstractScene;
import de.uniba.sme.bambirds.common.objects.SavedShot;
import de.uniba.sme.bambirds.common.objects.TargetPoint;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.shot.ShotPlanner;
import de.uniba.sme.bambirds.vision.Scene;

import static org.junit.Assert.*;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.imageio.ImageIO;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Before;

@RunWith(Parameterized.class)
public class ParabolaGenerationTest {
	private static final Logger log = LogManager.getLogger();

	private String imageFile;

	public ParabolaGenerationTest(String imageFile) {
		this.imageFile = imageFile;
	}

	@Before
	public void init(){
		Settings.load(new String[]{""});
	}

	@Test
	public void parabolasAreGenerated() throws IOException, SceneInitialisationException {

		BufferedImage img = ImageIO.read(getClass().getResource(imageFile));

		AbstractScene scene = new Scene(img);

		List<TargetPoint> targetPoints = scene.getTargetPoints();

		log.info(scene.getSlingshot());

		ShotPlanner planner = new ShotPlanner(scene);
		for (TargetPoint targetPoint : targetPoints) {
			scene.addPossibleShots(planner.savedShotsForTarget(targetPoint));
		}

		List<SavedShot> shots = scene.getPossibleShots();

		log.info(Arrays.toString(shots.toArray()));

		assertFalse("possible shots were empty", shots.isEmpty());

	}

	@Parameters
	public static Collection<String> data() {
		List<String> list = Arrays.asList("/images/sb-level0.png", "/images/sb-level1.png");

		return list;

	}

}