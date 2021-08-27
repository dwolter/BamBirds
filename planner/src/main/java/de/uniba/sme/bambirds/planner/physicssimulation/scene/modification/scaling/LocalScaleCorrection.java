package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.scaling;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
 
/**Apply a Manual Scale-Correction to all wood/stone/ice-rectangleblocks in the scene.
 *  By empirical research we found that the block-sizes in the game are always certain values ( which are stored in the sizeIntervalLUT ).
 *  (Empirical means here: making screenshots of the game, measuring pixels and the calculating).
 *  The modifier makes sure that the scale of those entities is the closest of those empirical values.
 *  WARNING 1: THIS IS AN ASSUMPTION AND MAY BE WRONG (although so far it seems to work out pretty well).
 *  WARNING 2: THE VALUES MAY NOT BE COMPLETE OR DISPROVEN by further empircal evidence.
 *  */

public class LocalScaleCorrection implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(LocalScaleCorrection.class);
    
    // how did we get the values ? we just measured the pixes from the screenshots maded by the agent.
    // we found that all objects in the first 20 levels fit into this dimension-sizes
    static int[] sizeIntervalLUT = new int[]{96,48,42,24,12,6}; //### // 
    // static int[] sizeIntervalLUT = new int[]{120,108,96,84,72,60,48,36,24,12,6}; //###

    public LocalScaleCorrection() {
    }

    @Override
    public void apply(Scene scene) {
        for (SceneEntityBase entity : scene.getAllEntities()) {
            if(entity.getAbType() == ABType.Ice || entity.getAbType() == ABType.Stone || entity.getAbType() == ABType.Wood ){

                if(entity instanceof SceneRectangle){
                    SceneRectangle sceneRectangle = (SceneRectangle) entity;
                    
                    if(isProbablyCubic(sceneRectangle)){
                        // do some manual hack here for smallest and secondsmallest blocks:
                        if(sceneRectangle.getWidth() >=9){
                            sceneRectangle.setWidth(12);
                            sceneRectangle.setHeight(12);
                        }else{
                            sceneRectangle.setWidth(6);
                            sceneRectangle.setHeight(6);
                        }

                    }else{
                        
                        int i = 0;
                        while(sceneRectangle.getWidth() < sizeIntervalLUT[i] && i < sizeIntervalLUT.length -1){
                            i++;
                        }
                        if(i == 0){
                            sceneRectangle.setWidth((sizeIntervalLUT[i]));
                        }else{
                            float distanceToPrev = Math.abs(sizeIntervalLUT[i-1] - sceneRectangle.getWidth());
                            float distanceToNext = Math.abs(sceneRectangle.getWidth() - sizeIntervalLUT[i]) ;
                            
                            if(distanceToPrev < distanceToNext){
                                i--;
                            }
                        }
                        sceneRectangle.setWidth((sizeIntervalLUT[i]));
                        
                        
                        i = 0;
                        while(sceneRectangle.getHeight() < sizeIntervalLUT[i] && i < sizeIntervalLUT.length -1){
                            i++;
                        }
                        if(i == 0){
                            sceneRectangle.setHeight((sizeIntervalLUT[i]));
                        }else{
                            float distanceToPrev = Math.abs(sizeIntervalLUT[i-1] - sceneRectangle.getHeight());
                            float distanceToNext = Math.abs(sceneRectangle.getHeight() - sizeIntervalLUT[i]) ;
                            
                            if(distanceToPrev < distanceToNext){
                                i--;
                            }
                        }
                        sceneRectangle.setHeight((sizeIntervalLUT[i]));
                    }
                }
            }
        }
    }
    
    boolean isProbablyCubic(SceneEntityBase entity){
        boolean isCubic = false;
        if(entity instanceof SceneRectangle){
            SceneRectangle sceneRectangle = (SceneRectangle) entity;
            if(sceneRectangle.getHeight() > 0f && sceneRectangle.getWidth() > 0f){

                float ratio = sceneRectangle.getHeight() / sceneRectangle.getWidth();
                if(ratio <1.25f && ratio > 0.75f){
                    isCubic = true;
                }

            }
        }else{
            isCubic = false;
        }
        return isCubic ;
    }
}