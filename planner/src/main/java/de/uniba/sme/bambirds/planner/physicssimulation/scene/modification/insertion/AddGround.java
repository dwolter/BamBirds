package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.insertion;

import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.BodyType;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.ScenePolygon;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/** Add a static horizontal ground-block to the scene which is sitting just below the lowest entity in the scene*/
public class AddGround implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(AddGround.class);
    

    @Override
    public void apply(Scene scene) {
        
        
        float groundPositionY = 0;
        if(scene.getHills().size() > 0){
            // if we have hills, then we assume ( hopefully right :) that all hills ( or at least one ) rests with his bottomside on the ground
            // we then use this info to place the ground just below the lowest hillpolygon
            float lowestHillVertexY = 0;
            for (ScenePolygon hill : scene.getHills()) {
                for(Vec2 vertex : hill.getVertices()){
                    if (vertex.y < lowestHillVertexY){
                        lowestHillVertexY = vertex.y ;
                    }
                }
            }
            groundPositionY = lowestHillVertexY;
        }else{
            // if we dont have hills, we use the lowest MBR-Bottom-Position that we can find in the scene as the ground-height
            float lowestPointOfAllEntities = 100000;
            for (SceneEntityBase sceneEntityBase : scene.getAllEntities()) {
                float entityPositionCenterY = (float) sceneEntityBase.getCenterY();
                float entityMBRHeight = (float) sceneEntityBase.getMbrHeight();
                // log.debug(" id= " + sceneEntityBase.getGlobalID()  + " Y= "+ entityPositionCenterY+ " Height= "+ entityMBRHeight);

               float lowestPointOfEntity =  entityPositionCenterY - (entityMBRHeight*0.5f);
               if(lowestPointOfEntity < lowestPointOfAllEntities){
                   lowestPointOfAllEntities = lowestPointOfEntity;
               }
            }
            groundPositionY = lowestPointOfAllEntities;
        }


        // now lets find the middlepoint of all objects in the scene
        Vec2 sceneCenterOfMass = new Vec2(0,0);
        for (SceneEntityBase sceneEntityBase : scene.getAllEntities()) {
            sceneCenterOfMass.x += sceneEntityBase.getCenterX();
            sceneCenterOfMass.y += sceneEntityBase.getCenterY();
        }
        if(scene.getAllEntities().size() > 0){
            sceneCenterOfMass.x  /= scene.getAllEntities().size();
            sceneCenterOfMass.y  /= scene.getAllEntities().size();
        }

        // log.debug("Lowest Point for GroundTop = " + groundPositionY);
        Rect rect = new Rect(sceneCenterOfMass.x, groundPositionY-1, 2, 30000, 0 , ABType.Ground );
        SceneRectangle groundRectangle = new SceneRectangle(rect);
        groundRectangle.setGlobalID("Ground_for_Simulation");
        groundRectangle.setId(-1);

        scene.addSceneEntity(groundRectangle);
    }
}