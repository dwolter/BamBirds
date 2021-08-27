package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Set rectangle-heights and reactangle-widths of all scene-rectangle-objects to the most common occuring values as long as thei are in maxDistance-margin*/
public class GlobalRectangleSizeSmoother implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(GlobalRectangleSizeSmoother.class);

    
    private float maxDistance;

    public GlobalRectangleSizeSmoother(float maxDistance) {
        this.maxDistance = maxDistance;
    }

    @Override
    public void apply(Scene scene) {
        HashMap<Float, Integer> heightCount = new HashMap<>();
        HashMap<Float, Integer> widthCount = new HashMap<>();

        count(scene, heightCount, widthCount);

        List<Float> mostCommonHeightValues = getMostCommonValues(heightCount);
        List<Float> mostCommonWidthValues = getMostCommonValues(widthCount);

        setMostCommonValues(scene, mostCommonHeightValues, mostCommonWidthValues);
    }

    private void setMostCommonValues(Scene scene, List<Float> mostCommonHeightValues, List<Float> mostCommonWidthValues) {
        for (SceneRectangle rectangle : scene.getAllSceneRectangles()) {
            for (Float height : mostCommonHeightValues) {
                if (rectangle.getHeight() > height - maxDistance && rectangle.getHeight() < height + maxDistance) {
                    rectangle.setHeight(height);;
                    break;
                }
            }
            for (Float width : mostCommonWidthValues) {
                if (rectangle.getWidth() > width - maxDistance && rectangle.getWidth() < width + maxDistance) {
                    rectangle.setWidth(width);;
                    break;
                }
            }
        }
    }

    private void count(Scene scene, HashMap<Float, Integer> heightCount, HashMap<Float, Integer> widthCount) {
        for (SceneRectangle rectangle : scene.getAllSceneRectangles()) {
            Float width = rectangle.getWidth();
            Float height = rectangle.getHeight();

            if (!widthCount.containsKey(width)) {
                widthCount.put(width, 1);
            } else {
                widthCount.put(width, widthCount.get(width) + 1);
            }
            if (!heightCount.containsKey(height)) {
                heightCount.put(height, 1);
            } else {
                heightCount.put(height, heightCount.get(height) + 1);
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