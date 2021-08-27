package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**attempts to align blocks in a scene such that they align on "straight invisible lines" when they are probably meant to be aligned that way but are not due to computer-vision-inaccuracies*/
public class BlockAlignment implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(BlockAlignment.class);

    private float maxDistance;

    public BlockAlignment(float maxDistance) {
        this.maxDistance = maxDistance;
    }

    @Override
    public void apply(Scene scene) {
        HashMap<Float, Integer> xPosCount = new HashMap<>();
        HashMap<Float, Integer> yPosCount = new HashMap<>();

        count(scene, xPosCount, yPosCount);

        List<Float> mostCommonXValues = getMostCommonValues(xPosCount);
        List<Float> mostCommonYValues = getMostCommonValues(yPosCount);

        setMostCommonValues(scene, mostCommonXValues, mostCommonYValues);
    }

    private void setMostCommonValues(Scene scene, List<Float> mostCommonXValues, List<Float> mostCommonYValues) {
        for (SceneRectangle rectangle : scene.getAllSceneRectangles()) {
            for (Float x : mostCommonXValues) {
                if (rectangle.getCenterX() > x - maxDistance && rectangle.getCenterX() < x + maxDistance) {
                    rectangle.setCenterX(x);
                    break;
                }
            }
            for (Float y : mostCommonYValues) {
                if (rectangle.getCenterY() > y - maxDistance && rectangle.getCenterY() < y + maxDistance) {
                    rectangle.setCenterY(y);
                    break;
                }
            }
        }
    }

    private void count(Scene scene, HashMap<Float, Integer> xCount, HashMap<Float, Integer> yCount) {
        for (SceneRectangle rectangle : scene.getAllSceneRectangles()) {
            Float x = rectangle.getCenterX();
            Float y = rectangle.getCenterY();

            if (!xCount.containsKey(x)) {
                xCount.put(x, 1);
            } else {
                xCount.put(x, xCount.get(x) + 1);
            }
            if (!yCount.containsKey(y)) {
                yCount.put(y, 1);
            } else {
                yCount.put(y, yCount.get(y) + 1);
            }
        }
    }

    private List<Float> getMostCommonValues(HashMap<Float, Integer> counts) {
        HashMap<Float, Integer> unclustered = new HashMap<>(counts);
        List<Float> mostCommonValues = new LinkedList<>();
        while (!unclustered.keySet().isEmpty()) {
            Float mostCommonValue = getMostCommenValues(unclustered);
            mostCommonValues.add(mostCommonValue);

            HashMap<Float, Integer> tmp = new HashMap<>();
            for (Float value : unclustered.keySet()) {
                if (value < (mostCommonValue - maxDistance) || value > (mostCommonValue + maxDistance))  {
                    tmp.put(value, counts.get(value));
                }
            }
            unclustered = tmp;
        }
        return mostCommonValues;
    }

    private Float getMostCommenValues(HashMap<Float, Integer> counts) {
        int biggestCount = 0;
        Float mostCommonValue = new Float(0.0f);
        for (Float value : counts.keySet()) {
            if (counts.get(value) > biggestCount) {
                biggestCount = counts.get(value);
                mostCommonValue = value;
            }
        }
        return mostCommonValue;
    }
    
}