package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation;

import java.util.List;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.ScenePolygon;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Translates all SceneEntities vertically such that the lowest MBR_Bottom rests exactly on the X-Axis. */
public class TranslationAboveHorizontalAxis implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(TranslationAboveHorizontalAxis.class);



    @Override
    public void apply(Scene scene) {
        float yOffset = Float.MAX_VALUE;

        for (SceneEntityBase entity : scene.getAllEntities()) {
            float distance = getDistanceFromLowestPointToXAxis(entity);
            yOffset = Math.min(yOffset, distance);
        }

        for (SceneEntityBase entity : scene.getAllEntities()) {
            entity.translate(0f, -yOffset);
        }

    }

    private float getDistanceFromLowestPointToXAxis(SceneEntityBase entity) {
        if (entity instanceof SceneRectangle) {
            return getDistanceFromRectangle((SceneRectangle) entity);
        } else if (entity instanceof SceneCircle) {
            return getDistanceFromCircle((SceneCircle) entity);
        } else if (entity instanceof ScenePolygon) {
            return getDistanceFromPolygon((ScenePolygon) entity);
        } else {
            // Ups?
            return 0f;
        }
    }

    private float getDistanceFromRectangle(SceneRectangle rect) {
        return rect.getCenterY() - (rect.getMbrHeight() / 2f);
    }
    
    private float getDistanceFromCircle(SceneCircle circle) {
        return circle.getCenterY() - circle.getRadius();
    }

    private float getDistanceFromPolygon(ScenePolygon poly) {
        return poly.getLowestVertexHeight();
    }
}