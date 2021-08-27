package de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing;

import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.insertion.AddGround;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.insertion.StoreSlingshotPosition;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.scaling.GlobalScale;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.scaling.LocalScaleCorrection;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing.AngleSmoother;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing.GlobalPolygonSmoother;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing.LocalPolygonSmoother;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation.BlockAlignment;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation.HorizontalMirror;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation.Translation;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.translation.TranslationAboveHorizontalAxis;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**The DefaultPreprocessor serves as a good startingpoint/example-implementation for preprocessing a raw scene that was created based on a Computervision-Level. 
 * It applies a finetuned sequence of SceneModifiers which works pretty well for the first 20 Levels of the Angry-Birds-Game. */
public class DefaultPreprocessor extends ScenePreprocessorBase {
    
    private static final Logger log = LogManager.getLogger(DefaultPreprocessor.class);

    public DefaultPreprocessor() {
        modifiers.add(new HorizontalMirror());
        modifiers.add(new TranslationAboveHorizontalAxis());
        modifiers.add(new AngleSmoother(10f));
        modifiers.add(new LocalPolygonSmoother(4));
        modifiers.add(new GlobalPolygonSmoother(4));
//        modifiers.add(new LocalScaleCorrection()); //may does some crazy stuff
        modifiers.add(new BlockAlignment(2f));

        //why 0.085f ? > because we found out empirically by playing and observing that the value is most likely 0.85f
        modifiers.add(new GlobalScale(0.085f));

        modifiers.add(new AddGround());
        modifiers.add(new StoreSlingshotPosition());
    }
}