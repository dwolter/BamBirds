package de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;

public class CustomPreProcessor extends ScenePreprocessorBase {
    
    public void addModifier(ISceneModifier modifier) {
        modifiers.add(modifier);
    }

}