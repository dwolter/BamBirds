package de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.smoothing;

import java.util.HashMap;

import org.jbox2d.common.Vec2;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.ScenePolygon;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Tries to counter inaccuracies from the Computervision.
 * It stores the amount of all occuring x-positions and y-positions of all vertices of all polygon-objects in the scene.
 * Some values may occur multiple times.
 * In a second pass the modifier will inspect all those vertices again, and whenever the x- or y-position of a vertex is within a close-distance-margin to one of those common values, the modifier will align the x- or y- values of all vertices within that range to one common value. It will try to align them to the most often-found value, if present. e.g. the vertices: [(5,10), (7,20), (6,21), (5,20), (4,20), (9,21)] with margin=4 will result in [(5,10),(5,20),(5,20),(5,20),(5,20),(10,20)].
 * It will help to make edges straight and on the same "line" throughout the scene across multiple seperate polygons. 
 * 
*/
public class GlobalPolygonSmoother implements ISceneModifier {
    private static final Logger log = LogManager.getLogger(GlobalPolygonSmoother.class);


    private int maxDistance;

    public GlobalPolygonSmoother(int maxDistance) {
        this.maxDistance = maxDistance;
    }

    @Override
    public void apply(Scene scene) {
        
        // first store the occuring x-and y-positions and their amounts of occurence.
        HashMap<Integer, Integer> xPositionsCount = new HashMap<>();
        HashMap<Integer, Integer> yPositionsCount = new HashMap<>();
        for(ScenePolygon polygon : scene.getAllScenePolygons()){
            
            for (int i = 0; i <  polygon.getVertices().length; i++) {
                Vec2 vertex = polygon.getVertices()[i];
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
            }
        }

        
        for(ScenePolygon polygon : scene.getAllScenePolygons()){
            for (int i = 0; i < polygon.getVertices().length; i++) {
                Vec2 vertex = polygon.getVertices()[i];

                int x = Math.round(vertex.x);
                int mostOccuredCountX  = 1;
                int  mostOccuredPositionX = x; // to always have at least any reasonable value, we take the value that the vertex already has as default
                for (int j = x - maxDistance; j <= x + maxDistance; j++){
                    if(xPositionsCount.containsKey(j) && xPositionsCount.get(j) >= mostOccuredCountX){
                        mostOccuredPositionX = j;
                        mostOccuredCountX = xPositionsCount.get(j);
                    }
                }
                vertex.x = mostOccuredPositionX;


                int y = Math.round(vertex.y);
                int mostOccuredCountY  = 1;
                int  mostOccuredPositionY = y; // to always have at least any reasonable value, we take the value that the vertex already has as default
                for (int j = y - maxDistance; j <= y + maxDistance; j++){
                    if(yPositionsCount.containsKey(j) && yPositionsCount.get(j) >= mostOccuredCountY){
                        mostOccuredPositionY = j;
                        mostOccuredCountY = yPositionsCount.get(j);
                    }
                }
                vertex.y = mostOccuredPositionY;
            }
        }

    }
    
}