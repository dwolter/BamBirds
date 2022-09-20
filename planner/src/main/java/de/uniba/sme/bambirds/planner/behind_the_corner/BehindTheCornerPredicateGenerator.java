package de.uniba.sme.bambirds.planner.behind_the_corner;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.FileUtil;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.predicates.IPredicateGenerator;
import de.uniba.sme.bambirds.planner.predicates.Predicate;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.Stream;

public class BehindTheCornerPredicateGenerator implements IPredicateGenerator {
    private static final Logger log = LogManager.getLogger(BehindTheCornerPredicateGenerator.class);

    private final AbstractScene scene;
    private final Level level;

    public BehindTheCornerPredicateGenerator(Level level) {
        this.scene = level.currentScene;
        this.level = level;
    }

    @Override
    public List<Predicate> call() throws PredicateGenerationException {
        String filename = "scene-representation-" + level.levelId + "-" + Math.abs(new Random().nextInt());
        String path = writeSceneRepresentation(filename);

        return Collections.singletonList(new Predicate("sceneRepresentation","\""+path.replace("\\", "\\\\")+"\""));
    }

    private String writeSceneRepresentation(String filename) {
        filename += "-rep";
        log.debug("Should now save scene representation to tmp/{}",filename);
        BufferedImage img = new BufferedImage(Settings.IMAGE_WIDTH, scene.getGroundPlane()+1,BufferedImage.TYPE_INT_RGB);
        Graphics2D g2d = img.createGraphics();

        // Draw the ground separately with normal vector (0,-1)
        g2d.setColor(new Color(ABType.Ground.id(), 127, 0));
        g2d.drawLine(0, scene.getGroundPlane(), Settings.IMAGE_WIDTH-1, scene.getGroundPlane());

        List<ABObject> all_Objects = new ArrayList<>();
        Stream.of(scene.getBlocks(), scene.getHills(), scene.getPigs(), scene.getTnts()).forEach(all_Objects::addAll);

        all_Objects.forEach(box -> box.drawRep(g2d));
        StringBuilder objectIDs = new StringBuilder();
        StringBuilder normsX = new StringBuilder();
        StringBuilder normsY = new StringBuilder();

        for (int y = 0; y< img.getHeight(); y++) {
            for (int x = 0; x < img.getWidth(); x++) {
                Color color = new Color(img.getRGB(x,y));

                int objectID = color.getRed();
                int normX = color.getGreen();
                int normY = color.getBlue();
                objectIDs.append(objectID).append(',');
                normsX.append(normX).append(',');
                normsY.append(normY).append(',');

            }
        }
        Path filePath = FileUtil.writeTemp(filename, String.valueOf(img.getWidth()) +
                ',' +
                img.getHeight() +
                "\n" +
                objectIDs +
                "\n" +
                normsX +
                "\n" +
                normsY);
        if (filePath == null) {
            log.error("Failed to write behind_the_corner scene representation");
        }
        return filePath.toString();
    }

    @Override
    public void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix) {

    }
}
