package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/**Mirrors all SceneEntities Vertically */

public class VerticalMirror implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(VerticalMirror.class);


    @Override
    public void apply(Scene scene) {
        for (SceneEntityBase entity : scene.getAllEntities()) {
            entity.mirrorOnYAxis();
        }
    }
    
}