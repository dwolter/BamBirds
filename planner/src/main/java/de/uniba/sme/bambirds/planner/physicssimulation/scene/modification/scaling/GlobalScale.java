package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.scaling;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Scale all objects in the scene by a scalingfactor. The scaling is performed relative to the origin of the global coordinate-system. */
public class GlobalScale implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(GlobalScale.class);

    
    private float scalingFactor;

    public GlobalScale(float scalingFactor) {
        this.scalingFactor = scalingFactor;
    }

    @Override
    public void apply(Scene scene) {
        for (SceneEntityBase entity : scene.getAllEntities()) {
            entity.scaleGlobally(scalingFactor);
        }
    }

}