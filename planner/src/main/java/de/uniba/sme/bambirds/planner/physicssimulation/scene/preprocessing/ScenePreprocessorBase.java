package de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing;

import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;

/**A ScenePreprocessor is a Container for a collection of SceneModifiers which will be called in sequence in one go.
 */
public class ScenePreprocessorBase implements ISceneModifier{
    
    public List<ISceneModifier> modifiers = new LinkedList<>();


    @Override
    public void apply(Scene scene) {
        for (ISceneModifier iSceneModifier : modifiers) {
            iSceneModifier.apply(scene);
        }
    }
}