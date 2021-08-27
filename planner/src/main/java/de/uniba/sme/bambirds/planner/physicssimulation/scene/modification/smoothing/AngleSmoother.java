package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/***
 * Tries to counter inaccuracies from the Computervision.
 * If the angle of any entity in the scene is close to either 0, 45, 90, 135 or 180 degrees (within the provided margin), then this Modifier will round the entities angle to one of the mentioned values. 
 * ASSUMPTION: This is based on the assumption that level-creators will often utilize these angles to position their objects, while the computervision may deliver inacurrate results, so we rount them to the (hopefully) "correct" results. 
 */
public class AngleSmoother implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(AngleSmoother.class);

    
    private float margin;

    public AngleSmoother(float marginInDegrees) {
        margin = marginInDegrees * ((float) Math.PI / 180f);
    }

    @Override
    public void apply(Scene scene) {
        for (SceneEntityBase entity : scene.getAllEntities()) {
            setToAngleIfInMargin(entity, 0f);
            setToAngleIfInMargin(entity, (float) Math.PI * 0.25f);
            setToAngleIfInMargin(entity, (float) Math.PI * 0.5f );
            setToAngleIfInMargin(entity, (float) Math.PI * 0.75f );
            setToAngleIfInMargin(entity, (float) Math.PI);

        }
    }

    private void setToAngleIfInMargin(SceneEntityBase entity, float targetAngle) {
        float angle = entity.getAngle();
        if (angle > targetAngle -margin  && angle < targetAngle + margin) {
            entity.setAngle(targetAngle);
        }
    }
    
}