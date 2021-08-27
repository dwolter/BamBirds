package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.insertion;

import java.util.List;

import org.jbox2d.common.Vec2;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Retrieve the position of the slingshot-cross-center and store it in the scene-objects field.
 * Currently the way to retrieve it is quite hacky, we use the position of the first bird in the birds-list,
 * because the actual Slingshot-object from the ComputerVision is null.
*/
public class StoreSlingshotPosition  implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(StoreSlingshotPosition.class);
    
	@Override
	public void apply(Scene scene) {
        List<SceneCircle> birds = scene.getBirds();
        if(birds != null &&  birds.size() > 0){
            SceneCircle referenceBird = birds.get(0);
            scene.slingShotCenterPosition = new Vec2(referenceBird.getCenterX(), referenceBird.getCenterY());
        }
	}
}
