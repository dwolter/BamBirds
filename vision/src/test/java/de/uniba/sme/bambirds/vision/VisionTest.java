package de.uniba.sme.bambirds.vision;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.vision.real.ImageSegmenter;

class VisionTest {

	@ParameterizedTest
	@CsvSource({
			// scene,redBirds,blueBirds,yellowBirds,blackBirds,whiteBirds
			"/scene1-1.png,  0,0,0,3,2",
			"/scene1-2.png,  0,0,0,2,2",
			"/scene1-3.png,  0,0,0,2,1",
			"/scene1-4.png,  0,0,0,1,1",
			"/scene1-5.png,  0,0,0,0,1",
			"/scene2-1.png,  0,3,1,0,0",
			"/scene2-2.png,  0,2,1,0,0",
			"/scene2-3.png,  0,1,1,0,0",
			"/scene2-4.png,  0,0,1,0,0",
			"/scene3-1.png,  4,0,1,0,0",
			"/scene3-2.png,  3,0,1,0,0",
			"/scene3-3.png,  2,0,1,0,0",
			"/scene3-4.png,  1,0,1,0,0",
			"/scene5-1.png,  0,4,0,0,0",
			"/scene5-2.png,  0,3,0,0,0",
			"/scene5-3.png,  0,2,0,0,0",
			"/scene7-1.png,  0,0,0,2,2",
			"/scene7-2.png,  0,0,0,2,1",
			"/scene7-3.png,  0,0,0,1,1",
			"/scene7-4.png,  0,0,0,0,1",
			"/scene11-1.png,  6,0,0,0,0",
			"/scene11-2.png,  5,0,0,0,0",
			"/scene11-3.png,  4,0,0,0,0",
			"/scene11-4.png,  3,0,0,0,0",
			"/scene11-5.png,  2,0,0,0,0",
			"/scene11-6.png,  1,0,0,0,0",
			"/scene12-1.png,  0,0,0,0,1",
			"/scene14-1.png,  1,0,1,0,1",
			"/scene14-2.png,  0,0,1,0,1",
			"/scene14-3.png,  0,0,1,0,0",
			"/scene16-1.png,  1,1,1,0,0",
			"/scene16-2.png,  1,1,0,0,0",
			"/scene17-1.png,  0,0,0,0,4",
			"/original-1/scene1-1.png, 3,0,0,0,0",
			"/original-1/scene2-1.png, 5,0,0,0,0",
			"/original-1/scene3-1.png, 4,0,0,0,0",
			"/original-1/scene3-2.png, 3,0,0,0,0",
			"/original-1/scene4-1.png, 4,0,0,0,0",
			"/original-1/scene4-2.png, 3,0,0,0,0",
			"/original-1/scene4-3.png, 2,0,0,0,0",
			"/original-1/scene5-1.png, 4,0,0,0,0",
			"/original-1/scene5-2.png, 3,0,0,0,0",
			"/original-1/scene5-3.png, 2,0,0,0,0",
			"/original-1/scene5-4.png, 1,0,0,0,0",
			"/original-1/scene6-1.png, 4,0,0,0,0",
			"/original-1/scene6-2.png, 3,0,0,0,0",
			"/original-1/scene6-3.png, 2,0,0,0,0",
			"/original-1/scene6-4.png, 1,0,0,0,0",
			"/original-1/scene7-1.png, 4,0,0,0,0",
			"/original-1/scene7-2.png, 3,0,0,0,0",
			"/original-1/scene8-1.png, 4,0,0,0,0",
			"/original-1/scene8-2.png, 4,0,0,0,0", // One birds still rolling
			"/original-1/scene8-3.png, 2,0,0,0,0",
			"/original-1/scene8-4.png, 1,0,0,0,0",
			"/original-1/scene9-1.png, 4,0,0,0,0",
			"/original-1/scene10-1.png, 0,5,0,0,0",
			"/original-1/scene10-2.png, 0,4,0,0,0",
			"/original-1/scene10-3.png, 0,3,0,0,0",
			"/original-1/scene10-4.png, 0,2,0,0,0",
			"/original-1/scene10-5.png, 0,1,0,0,0",
			"/original-1/scene11-1.png, 1,3,0,0,0",
			"/original-1/scene11-2.png, 0,3,0,0,0",
			"/original-1/scene12-1.png, 1,3,0,0,0",
			"/original-1/scene12-2.png, 1,2,0,0,0",
			"/original-1/scene13-1.png, 2,2,0,0,0",
			"/original-1/scene14-1.png, 4,0,0,0,0",
			"/original-1/scene14-2.png, 3,0,0,0,0",
			"/original-1/scene14-3.png, 2,0,0,0,0",
			"/original-1/scene14-4.png, 1,0,0,0,0",
			"/original-1/scene15-1.png, 0,4,0,0,0",
			"/original-1/scene15-2.png, 0,3,0,0,0",
			"/original-1/scene16-1.png, 0,0,5,0,0",
			"/original-1/scene16-2.png, 0,0,4,0,0",
			"/original-1/scene16-3.png, 0,0,3,0,0",
			"/original-1/scene16-4.png, 0,0,2,0,0",
			"/original-1/scene17-1.png, 0,0,3,0,0",
			"/original-1/scene17-2.png, 0,0,2,0,0",
			"/original-1/scene18-1.png, 0,0,5,0,0",
			"/original-1/scene18-2.png, 0,0,4,0,0",
			"/original-1/scene18-3.png, 0,0,3,0,0",
			"/original-1/scene18-4.png, 0,0,2,0,0",
			"/original-1/scene18-5.png, 0,0,1,0,0",
			"/original-1/scene19-1.png, 1,1,2,0,0",
			"/original-1/scene19-2.png, 1,0,2,0,0",
			"/original-1/scene19-3.png, 1,0,1,0,0",
			"/original-1/scene19-4.png, 1,0,0,0,0",
			"/original-1/scene20-1.png, 0,0,5,0,0",
			"/original-1/scene20-2.png, 0,0,4,0,0",
			"/original-1/scene20-3.png, 0,0,3,0,0",
			"/original-1/scene20-4.png, 0,0,2,0,0",
			"/original-1/scene21-1.png, 2,2,4,0,0",
			"/original-1/scene21-2.png, 2,1,4,0,0",
			"/original-1/scene21-3.png, 1,1,4,0,0",
			"/original-1/scene21-4.png, 1,1,3,0,0",
			"/original-1/scene21-5.png, 1,0,3,0,0",
			"/original-1/scene21-6.png, 0,0,3,0,0",
			"/original-1/scene21-7.png, 0,0,2,0,0",
			"/original-1/scene21-8.png, 0,0,1,0,0",
			"/scene_init_failed-3-3.png, 0,0,1,0,0",
	})
	@EnabledIfEnvironmentVariable(named = "BAMBIRDS_VISION_TEST", matches = "true")
	void findBirds(String sceneFile, int numRedBirds, int numBlueBirds, int numYellowBirds, int numBlackBirds, int numWhiteBirds) throws IOException {
		BufferedImage img = ImageIO.read(getClass().getResource(sceneFile));
		img = ImageUtil.removeABUI(img, 0);
		Vision v = new Vision(img);

		List<ABObject> pigs = v.findPigsRealShape();
		List<ABObject> birds = v.findBirdsRealShape();
		List<ABObject> redBirds = birds.stream().filter(a -> a.getType() == ABType.RedBird).collect(Collectors.toList());
		List<ABObject> blueBirds = birds.stream().filter(a -> a.getType() == ABType.BlueBird).collect(Collectors.toList());
		List<ABObject> yellowBirds = birds.stream().filter(a -> a.getType() == ABType.YellowBird).collect(Collectors.toList());
		List<ABObject> blackBirds = birds.stream().filter(a -> a.getType() == ABType.BlackBird).collect(Collectors.toList());
		List<ABObject> whiteBirds = birds.stream().filter(a -> a.getType() == ABType.WhiteBird).collect(Collectors.toList());

		if (numRedBirds != redBirds.size() || numBlueBirds != blueBirds.size() || numYellowBirds != yellowBirds.size() || numBlackBirds != blackBirds.size() || numWhiteBirds != whiteBirds.size()) {
			BufferedImage edges = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			BufferedImage components = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			BufferedImage classification = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			BufferedImage birds_image = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			VisionUtils.drawBoundingBoxes(birds_image, birds, null);
			ImageSegmenter seg = new ImageSegmenter(img);
			seg.drawClassification(classification);
			ImageUtil.saveToDebugFile(classification, sceneFile.replace(".png", "-classificationBefore"));
			seg.findComponents();
			seg.drawEdges(edges);
			seg.drawComponents(components, false);
			seg.drawClassification(classification);
			ImageUtil.saveToDebugFile(edges, sceneFile.replace(".png", "-edges"));
			ImageUtil.saveToDebugFile(components, sceneFile.replace(".png", "-components"));
			ImageUtil.saveToDebugFile(classification, sceneFile.replace(".png", "-classification"));
			ImageUtil.saveToDebugFile(birds_image, sceneFile.replace(".png", "-birds"));
			
		}

		assertEquals(numRedBirds, redBirds.size(), "Number of red birds should be equal");
		assertEquals(numBlueBirds, blueBirds.size(), "Number of blue birds should be equal");
		assertEquals(numYellowBirds, yellowBirds.size(), "Number of yellow birds should be equal");
		assertEquals(numBlackBirds, blackBirds.size(), "Number of black birds should be equal");
		assertEquals(numWhiteBirds, whiteBirds.size(), "Number of white birds should be equal");
	}


	@ParameterizedTest
	@CsvSource({
			// scene,tnt_count
			"/scene20-1.png,  3",
	})
	@EnabledIfEnvironmentVariable(named = "BAMBIRDS_VISION_TEST", matches = "true")
	void findTnts(String sceneFile, int tnt_count) throws IOException {
		BufferedImage img = ImageIO.read(getClass().getResource(sceneFile));
		img = ImageUtil.removeABUI(img, 0);
		Vision v = new Vision(img);

		List<ABObject> tnts = v.findTNTs();

		if (tnt_count != tnts.size()) {
			BufferedImage edges = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			BufferedImage components = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			BufferedImage classification = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			BufferedImage tnt_image = new BufferedImage(img.getWidth(), img.getHeight(), BufferedImage.TYPE_INT_RGB);
			VisionUtils.drawBoundingBoxes(tnt_image, tnts, null);
			ImageSegmenter seg = new ImageSegmenter(img);
			seg.drawClassification(classification);
			ImageUtil.saveToDebugFile(classification, sceneFile.replace(".png", "-classificationBefore"));
			seg.findComponents();
			seg.drawEdges(edges);
			seg.drawComponents(components, false);
			seg.drawClassification(classification);
			ImageUtil.saveToDebugFile(edges, sceneFile.replace(".png", "-edges"));
			ImageUtil.saveToDebugFile(components, sceneFile.replace(".png", "-components"));
			ImageUtil.saveToDebugFile(classification, sceneFile.replace(".png", "-classification"));
			ImageUtil.saveToDebugFile(tnt_image, sceneFile.replace(".png", "-tnts"));

		}

		assertEquals(tnt_count, tnts.size(), "Number of TNTs should be equal");
	}

}
