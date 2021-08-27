package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing;

import java.util.HashMap;

import org.jbox2d.common.Vec2;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.ScenePolygon;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;



/** attempts to counter computer-vision-inaccuracies.
 * The modifier will go through each polygon one by one. It will try to to fight small inaccuracies and make edges straighter.
 * If multiple vertices are probably meant to sit on an "invisible line" in x-direction or y-direction (whithin the given maxdistance), the vertices will be shifted to the most occuring value on that "line". If there are only 2 vertices within that distance, one of them will be shifted to the others value ( by random choice ), so that they form a straight edge.
 */
public class LocalPolygonSmoother implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(LocalPolygonSmoother.class);


    private int maxDistance;

    public LocalPolygonSmoother(int maxDistance) {
        this.maxDistance = maxDistance;
    }

    @Override
    public void apply(Scene scene) {
        for (ScenePolygon poly : scene.getAllScenePolygons()) {
            smoothPolygon(poly);
        }
    }

    private void smoothPolygon(ScenePolygon poly) {
        Vec2[] vertices = poly.getVertices();
        
        HashMap<Integer, Integer> xPositionsCount = new HashMap<>();
        HashMap<Integer, Integer> yPositionsCount = new HashMap<>();
        Vec2 polygonCenter = new Vec2(0,0);

        for (int i = 0; i < vertices.length; i++) {
            Vec2 vertex = vertices[i];
            int x = (int) Math.round(vertex.x);
            int y = (int) Math.round(vertex.y);
            if(xPositionsCount.containsKey(x) == false){
                xPositionsCount.put( x, 0);
            }
            xPositionsCount.put(x, xPositionsCount.get(x) +1);
            
            if(yPositionsCount.containsKey(y) == false){
                yPositionsCount.put( y, 0);
            }
            yPositionsCount.put(y, yPositionsCount.get(y) +1);

            polygonCenter.x += x;
            polygonCenter.y += y;
        }
        if(vertices.length > 0){
            polygonCenter.x /= vertices.length;
            polygonCenter.y /= vertices.length;
        }



        for (int i = 0; i < vertices.length; i++) {
            Vec2 vertex = vertices[i];

            int x = Math.round(vertex.x);
            int mostOccuredCountX  = 1;
            int  mostOccuredPositionX = x; // to always have at least any reasonable value, we take the value that the vertex already has as default
            // If it is hard to decide, We always try to pick the one which is closer to the center of the polygon
            for (int j = x - maxDistance; j <= x + maxDistance; j++){
                if(xPositionsCount.containsKey(j) && (
                     (xPositionsCount.get(j) > mostOccuredCountX) ||
                     (xPositionsCount.get(j) ==  mostOccuredCountX && xPositionsCount.get(j)- polygonCenter.x < mostOccuredPositionX - polygonCenter.x)
                )
                
                ){
                    mostOccuredPositionX = j;
                    mostOccuredCountX = xPositionsCount.get(j);
                }
            }
            vertex.x = mostOccuredPositionX;


            int y = Math.round(vertex.y);
            int mostOccuredCountY  = 1;
            int  mostOccuredPositionY = y; // to always have at least any reasonable value, we take the value that the vertex already has as default
            for (int j = y - maxDistance; j <= y + maxDistance; j++){
                if(yPositionsCount.containsKey(j) && 
                (   (yPositionsCount.get(j) > mostOccuredCountY) ||
                    (yPositionsCount.get(j) ==  mostOccuredCountY && yPositionsCount.get(j)- polygonCenter.y < mostOccuredPositionY - polygonCenter.y)
                ))
                {
                    mostOccuredPositionY = j;
                    mostOccuredCountY = yPositionsCount.get(j);
                }
            }
            vertex.y = mostOccuredPositionY;
        }
    }


    
}