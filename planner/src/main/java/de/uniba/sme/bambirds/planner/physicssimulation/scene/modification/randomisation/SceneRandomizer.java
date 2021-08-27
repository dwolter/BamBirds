package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.randomisation;

import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;

public class SceneRandomizer implements ISceneModifier {

    float PositionX;
    float PositionY;
    float RotationAngles;
    float Scale;

    Random random;
    

    public SceneRandomizer(float positionX, float positionY, float rotationAngles, float scale) {
        PositionX = positionX;
        PositionY = positionY;
        RotationAngles = rotationAngles;
        Scale = scale;
        random = new Random();
    }


    @Override 
	public void apply(Scene scene) {
        for(SceneEntityBase sceneEntityBase : scene.getAllEntities()){
            switch(sceneEntityBase.getAbType()){
                case Hill:
                case Ice:
                case Stone:
                case Wood:
                case Pig: 
                    applyRandomizationToEntity(sceneEntityBase);
                    break;
                default:
                    break;
            }
        }
    }
    
    /**
     * Generate a List of Randomized Scenes based on an inputScene. The initialScene is included in the list at Index 0
     * @param initialScene The sourceScene from which the other randomization will be derived  
     * @param cloneAmount How many additional Scenes do you want ?
     * @return
     */
    public List<Scene> generateRandomizedSceneClones(Scene initialScene, int cloneAmount){
        List<Scene> scenes = new LinkedList<Scene>();
        scenes.add(initialScene);
        for (int i = 0; i < cloneAmount; i++) {
            Scene scene = new Scene(initialScene);
            apply(scene);
        }
        return scenes;
    }




    /**
     * Generates a Random Float between -1 and 1
     * @return
     */
    float getRandomFloatPlusMinusOne(){
        return  (random.nextFloat() - 0.5f) *2f;
    }

    void applyRandomizationToEntity(SceneEntityBase sceneEntityBase){
        sceneEntityBase.translate(getRandomFloatPlusMinusOne() * PositionX , getRandomFloatPlusMinusOne() * PositionY);
        sceneEntityBase.rotateLocally(getRandomFloatPlusMinusOne() * RotationAngles);
        sceneEntityBase.scaleLocally(getRandomFloatPlusMinusOne() * Scale);
    }
}
