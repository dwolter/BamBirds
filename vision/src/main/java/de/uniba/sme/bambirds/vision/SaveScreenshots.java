package de.uniba.sme.bambirds.vision;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.Color;
import java.awt.image.BufferedImage;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;

/**
 * SaveScreenshots
 */
public class SaveScreenshots {
	private static final Logger log = LogManager.getLogger();
	private static VisualDebugger DBG = new VisualDebugger("Scene");
	{ DBG.enableDebug(false, true); }

	// TODO: saving screenshots should be moved to utils or debugging
	public static void writeScreenshotWithIDs(AbstractScene scene, String filename) {
		log.debug("Should now save screenshots to {}",filename);
		if (DBG.canSaveFile()) {
			DBG.saveToFileDirectly(filename, scene.getImage());
			writeScreenshotWithIDs(scene.getImage(), filename, scene.getPigs(), "pigs", Color.RED);
			writeScreenshotWithIDs(scene.getImage(), filename, scene.getBlocks(), "blocks", Color.MAGENTA);
			writeScreenshotWithIDs(scene.getImage(), filename, scene.getHills(), "hills", Color.ORANGE);
			List<ABObject> slingshotList = new ArrayList<>();
			slingshotList.add(scene.getSlingshot());
			writeScreenshotWithIDs(scene.getImage(), filename, slingshotList, "slingshot", Color.BLUE);
			writeScreenshotWithIDs(scene.getImage(), filename, scene.getBirds(), "birds", Color.BLUE);
		}
	}

	public static void writeOneScreenshotWithIDs(AbstractScene scene, String filename) {
		log.debug("Should now save screenshots to {}",filename);
		if (DBG.canSaveFile()) {
			writeScreenshotWithIDs(scene.getImage(), filename, scene.getPigs(), "pigs", Color.RED);
		}
	}

	private static void writeScreenshotWithIDs(BufferedImage image, String filename, List<ABObject> objects, String type, Color color) {
		VisionUtils.drawBoundingBoxesWithID(image, objects, color);
		DBG.saveToFileDirectly(filename + "-" + type, image);
	}
}
