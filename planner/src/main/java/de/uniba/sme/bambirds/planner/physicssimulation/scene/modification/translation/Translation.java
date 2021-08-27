package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/**Translates all ScenEntites globally by the provided offset*/
public class Translation implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(Translation.class);


    private float xOffset;
    private float yOffset;

    public Translation(float xOffset, float yOffset) {
        this.xOffset = xOffset;
        this.yOffset = yOffset;
    }

    @Override
    public void apply(Scene scene) {
        for (SceneEntityBase entity : scene.getAllEntities()) {
            entity.translate(xOffset, yOffset);
        }
    }
    
}