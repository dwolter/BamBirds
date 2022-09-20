package de.uniba.sme.bambirds.feedback;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import javax.imageio.ImageIO;

import java.awt.image.BufferedImage;
import java.io.IOException;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.vision.Scene;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

public class SequenceCalculatorIntegrationTest {

  @BeforeAll
  public static void init() {
    Settings.DEBUG_DIR = Paths.get("debug");
    Settings.DEBUG_DIR.toFile().mkdirs();
  }
  
  @Test
  @Disabled("Not stable")
  public void level1shot1Test() throws IOException, SceneInitialisationException {
    String[] imagePaths = new String[] {
      "/situation4-1.png",
      "/intermediate-level4-shot1-0.png",
      "/intermediate-level4-shot1-1.png",
      "/intermediate-level4-shot1-2.png",
      "/intermediate-level4-shot1-3.png",
      "/intermediate-level4-shot1-4.png",
      "/intermediate-level4-shot1-5.png",
    };
    AbstractScene firstScene = new Scene(ImageIO.read(getClass().getResource("/situation4-1.png")));
    List<ABObject> objects = new ArrayList<>();
    objects.addAll(firstScene.getBlocks());
    objects.addAll(firstScene.getPigs());
    int[] cutoff = {-1, -1, -1, -1};
    if (!objects.isEmpty()) {
      // Min X
      cutoff[0] = Math.max(0,(int) objects.stream().min(Comparator.comparing(ABObject::getCenterX)).get().getCenterX() - 80);
      // Max X
      cutoff[1] = Math.min(Settings.IMAGE_WIDTH,(int) objects.stream().max(Comparator.comparing(ABObject::getCenterX)).get().getCenterX() + 80);
      // Min Y
      cutoff[2] = Math.max(0,(int) objects.stream().min(Comparator.comparing(ABObject::getCenterY)).get().getCenterY() - 30);
      // Max Y
      cutoff[3] = firstScene.getGroundPlane();
    }
    List<FeedbackScene> feedbacklist = Arrays.stream(imagePaths).parallel().map(path -> {
      try {
        BufferedImage image = ImageIO.read(getClass().getResource(path));
        if (image == null) {
          throw new Exception("Image null");
        }
        int sceneCounter = Integer.parseInt(path.substring(path.indexOf(".png") - 1, path.indexOf(".png")));
        SceneCalculator calculator = new SceneCalculator(image, ImageUtil.removeABUIAndClip(image, cutoff), cutoff, sceneCounter);
        return calculator.call();
      } catch (Exception e) {
        e.printStackTrace();
        return null;
      }
    }).collect(Collectors.toList());
    SequenceCalculator sequenceCalculator = new SequenceCalculator(feedbacklist);
    sequenceCalculator.calculate();

    SequenceCalculatorDebugging.drawSequencesOnImages(feedbacklist, sequenceCalculator.getUnmovedObjects(), sequenceCalculator.getMovedObjects(), sequenceCalculator.getDestroyedObjects());

    Assertions.assertEquals(3,sequenceCalculator.getDestroyedObjects().size(), "Should have the same amount of destroyed objects");
    Assertions.assertEquals(5,sequenceCalculator.getMovedObjects().size(), "Should have the same amount of moved objects");
    Assertions.assertEquals(50,sequenceCalculator.getUnmovedObjects().size(), "Should have the same amount of unmoved objects");
  }
}
