package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;


/** A SceneModifier implements a function that applies arbitrary changes to the state of a Scene-Object */
public interface ISceneModifier {
    
    /**
     * Apply arbitrary changes to the state of a Scene-Object 
     * @param scene
     */
    public void apply(Scene scene);
    
}