package de.uniba.sme.bambirds.feedback;

import java.util.List;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.image.BufferedImage;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.utils.ImageUtil;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class SequenceCalculatorDebugging {
    private static final Logger log = LogManager.getLogger(SequenceCalculatorDebugging.class);

    protected static void drawSequencesOnImages(List<FeedbackScene> sceneList, List<SequenceObject> unmoved, 
    List<SequenceObject> moved, List<SequenceObject> destroyed){
        for(FeedbackScene scene : sceneList){
            /* grey image, objects with red border, colorful sequence lines, ids */
            drawObjectsOnImage(scene.drawImage(true, true, Color.RED), scene,unmoved, "unmoved/unmoved-" + scene.getId(), true, false);
            drawObjectsOnImage(scene.drawImage(true, true, Color.RED), scene,moved, "moved/moved-" + scene.getId(), true, false);
            drawObjectsOnImage(scene.drawImage(true, true, Color.RED), scene,destroyed, "destroyed/destroyed-" + scene.getId(), true, false);
            /* normal image, objects with black border, colorful sequence lines, no ids */
            drawObjectsOnImage(scene.drawImage(true, false, Color.BLACK), scene,unmoved, "unmoved/objects-" + scene.getId(), true, true);
            drawObjectsOnImage(scene.drawImage(true, false, Color.BLACK), scene,moved, "moved/objects-" + scene.getId(), true, true);
            drawObjectsOnImage(scene.drawImage(true, false, Color.BLACK), scene,destroyed, "destroyed/objects-" + scene.getId(), true, true);
        }
    }

    private static void drawObjectsOnImage(BufferedImage image, FeedbackScene scene, List<SequenceObject> sequenceList, String name, boolean drawColor, boolean drawObjects){
        for (SequenceObject sequence : sequenceList) {
            ABObject tmpObject = sequence.getObjectList().get(0);
            Point2D point1 = new Point2D.Double(tmpObject.getCenterX(), tmpObject.getCenterY());
            Point2D point2 = (new Point2D.Double(tmpObject.getCenterX() -scene.getTranslateX(), tmpObject.getCenterY() -scene.getTranslateY()));
            Color color;
            if(drawColor == true){
                color = calculateRandomColor((long) sequence.getGlobalID());
            }else{
                color = Color.GREEN;
            }
            for (ABObject object : sequence.getObjectList()) {
                if (object != null) {
                    if(drawObjects == true){
                        Graphics2D g = image.createGraphics();
                        object.translate(-scene.getTranslateX(),-scene.getTranslateY());
                        object.draw(g,false, color);
                        object.translate(scene.getTranslateX(),scene.getTranslateY());
                    }else{
                        point1 = point2;
                        point2 = new Point2D.Double(object.getCenterX()-scene.getTranslateX(), object.getCenterY() -scene.getTranslateY());
                        drawLineToImage(image, point1, point2, color);
                    }
                }
            }
        }
        ImageUtil.saveToDebugFile(image, name);
    }

    private static void drawLineToImage(BufferedImage image, Point2D point1, Point2D point2, Color color) {
        Graphics2D g = image.createGraphics();
        Line2D line = new Line2D.Double(point1, point2);
        Ellipse2D circle1 = new Ellipse2D.Double(point1.getX(), point1.getY(), 2, 2);
        Ellipse2D circle2 = new Ellipse2D.Double(point2.getX(), point2.getY(), 2, 2);
        g.setColor(color);
        g.draw(line);
        g.fill(circle1);
        g.fill(circle2);
    }

    private static Color calculateRandomColor(long id){
        double pseudoRandom = (double) ((Math.pow(id+1,5))%43)/42;
        float hue = (float)(pseudoRandom *7+1.5)/10;
        float saturation = 0.9f;
        float luminance = 0.9f;
        return Color.getHSBColor(hue, saturation, luminance);
    }

    protected static void printSequences( List<SequenceObject> unmoved, List<SequenceObject> moved, List<SequenceObject> destroyed){
        Path path = Paths.get("debug/sequences.txt");
        try(BufferedWriter writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)){
            writer.write("###### Unmoved Objects ######\n\n");
            writer.write(sequenceListToReadableString(unmoved));
            writer.write("\n\n###### Moved Objects ######\n\n");
            writer.write(sequenceListToReadableString(moved));
            writer.write("\n\n###### Destroyed Objects ######\n\n");
            writer.write(sequenceListToReadableString(destroyed));
            writer.close();
        } catch (IOException e) {
            log.info("[SequenceCalculatorDebugging] Could not write sequences into file for debugging.");
        }
    }
    private static String sequenceListToReadableString(List<SequenceObject> sequenceList){
        String string = "";
        for(SequenceObject sequence : sequenceList){
            string += sequence.toString() + "\n";
        }
        return string;
    }

    protected static void printObjects(List<FeedbackScene> sceneList){
        Path path = Paths.get("debug/objects.txt");
        try(BufferedWriter writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)){
            for(FeedbackScene scene: sceneList){
                String objectListString = "";

                for(ABObject object : scene.getObjects()){
                    objectListString += ABObjectHelper.writeABObjectAsString(object);
                }
                writer.write("\n###### Scene "+scene.getId()+" ######\n\n");
                writer.write(objectListString);
                
            }
            writer.close();
        } catch (IOException e) {
            log.info("[SequenceCalculatorDebugging] Could not write objects into file for debugging.");
        }
    }

    protected static void printSplitValues(Map<ABType,List<Double>> splitValuesOfType) {
        Path path = Paths.get("debug/splitValues.txt");
        try(BufferedWriter writer = Files.newBufferedWriter(path, StandardCharsets.UTF_8)){
            writer.write(splitValuesOfType.toString());
            writer.close();
        } catch (IOException e) {
            log.info("[SequenceCalculatorDebugging] Could not write objects into file for debugging.");
        }
    }
}