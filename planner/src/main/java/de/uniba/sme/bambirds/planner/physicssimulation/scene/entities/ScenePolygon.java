package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities;

import java.awt.Point;
import java.awt.Polygon;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedList;

import org.jbox2d.collision.shapes.PolygonShape;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyDef;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.LineSegment;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Used for SceneEntites that have a  polygonshape ( defined by lists of Vertices ) > currently only Objects with ABType "hills" have a Polygonshape */
public class ScenePolygon extends SceneEntityBase {
    private static final Logger log = LogManager.getLogger(ScenePolygon.class);


    private static final long serialVersionUID = 1L;

    private Vec2[] vertices;
    private Poly originalPoly;


     /**
     * Function to Copy all State ( attribute-values ) from this entity to another entity
     * @param targetEntity
     */
    public void CopyEntityState(ScenePolygon targetEntity){
        super.CopyEntityState(targetEntity);
        targetEntity.originalPoly = originalPoly;
        targetEntity.vertices = new Vec2[vertices.length];
        for (int i = 0; i < vertices.length; i++) {
            targetEntity.vertices[i] = new Vec2(vertices[i]);
        }
    }

      /**
     * This Constructor is meant as a copyconstructor. you can put in a ScenePolygon and it will return a new ScenePolygon with an equal state
     * @param sourceScenePolygon
     */
    public ScenePolygon(ScenePolygon sourceScenePolygon){
        super(sourceScenePolygon.originalPoly);
        sourceScenePolygon.CopyEntityState(this);
    }
    /**
     * This Constructor is meant for creating a ScenePolygon-Entity out of a BodyObject of type Poly from the computervisions inputdata
     * @param poly
     * NOTE: Box2D can only handle collision for CONVEX shapes NOTE: Vertices are
     * expected in CLOCKWISE order NOTE: JBox2D implementation can only handle
     * Polygons that are CONVEX and have a MAXIMUM OF 8 Vertices ( otherwise you need to split the polygons )
     */
    public ScenePolygon(Poly poly) {
        super(poly);
        this.originalPoly = poly;
        
        Polygon polygon = poly.polygon;
        vertices = new Vec2[polygon.npoints];
        
        for (int i = 0; i < vertices.length; i++) {
            vertices[i] = new Vec2((float) polygon.xpoints[i], (float) polygon.ypoints[i]);
        }
    }


    public static ScenePolygon createScenePolygon(String globalID, int id, ABType abType,  float offsetX, float offsetY, float angle, Vec2[] vertices ){
        ArrayList<LineSegment> lineSegments = new ArrayList<>();
        for (int i = 0; i < vertices.length; i++) {
            lineSegments.add(new LineSegment(new Point((int)vertices[i].x, (int)vertices[i].y), 0));
        }
        Poly poly = new Poly(lineSegments, 0, 0, abType, offsetX, offsetY);
        ScenePolygon scenePolygon = new ScenePolygon(poly);
        scenePolygon.setGlobalID(globalID);
        scenePolygon.setId(id);
        scenePolygon.translate(offsetX, offsetY);
        scenePolygon.rotateLocally(angle);
        return scenePolygon;
    }

    @Override
    public String toString() {
        String s = super.toString() + "\n" + "Polygon with Vertices Amount: " + vertices.length;

        //------- uncomment this if you want to include all vertices in the output: -------//
        // for (int i = 0; i < vertices.length; i++) {
        //     s += "\tVertex " + i + " : (" + vertices[i].x + " | " + vertices[i].y + " )\n";
        // }
        return s;
    }

    public static double getAngleBetweenVectors(Vec2 a, Vec2 b ){
        return Vec2.cross(a, b);
    }

    /**
     * when connecting the first 2 vertices of a vertex-array from vertex1 to vertex2 and forming a line, is a point X to the left of that line ?
     */
    public boolean isPointLeftOfEdgeVector(Vec2 point, Vec2[] edgeVertices){
        Vec2 a = new Vec2( point.x - edgeVertices[0].x ,point.y - edgeVertices[0].y );
        Vec2 b = new Vec2( edgeVertices[1].x - edgeVertices[0].x , edgeVertices[1].y - edgeVertices[0].y );
        double angle = getAngleBetweenVectors(a, b);
        return angle > 0;
    }
    /**
     * when connecting the first 2 vertices of a vertex-array from vertex1 to vertex2 and forming a line, is a point X to the right of that line ?
     */
    public boolean isPointRightOfEdgeVector(Vec2 point, Vec2[] edgeVertices){
        Vec2 a = new Vec2( point.x - edgeVertices[0].x ,point.y - edgeVertices[0].y );
        Vec2 b = new Vec2( edgeVertices[1].x - edgeVertices[0].x , edgeVertices[1].y - edgeVertices[0].y );
        double angle = getAngleBetweenVectors(a, b);
        return angle < 0;
    }

    public boolean isPointInsideOfTriangle(Vec2 point, Vec2[] triangleEdgeVertices){
        // TODO Question ? > WHat if the point is Exactly on an Edge ?
            return
            isPointLeftOfEdgeVector(point, new Vec2[]{triangleEdgeVertices[0], triangleEdgeVertices[1]}) &&
            isPointLeftOfEdgeVector(point, new Vec2[]{triangleEdgeVertices[1], triangleEdgeVertices[2]}) &&
            isPointLeftOfEdgeVector(point, new Vec2[]{triangleEdgeVertices[2], triangleEdgeVertices[0]})
            ||
            isPointRightOfEdgeVector(point, new Vec2[]{triangleEdgeVertices[0], triangleEdgeVertices[1]}) &&
            isPointRightOfEdgeVector(point, new Vec2[]{triangleEdgeVertices[1], triangleEdgeVertices[2]}) &&
            isPointRightOfEdgeVector(point, new Vec2[]{triangleEdgeVertices[2], triangleEdgeVertices[0]});
    }


    @Override
    public void addToWorld(World world) {
        // Whenever we work with polygons, we use a simple algorithm to split the the polygons into a set of triangles ("triangulation").
        // For each of those triangles we instanciate a seperate body into the jbox2d-world and set it to being static (Because so far we found that all objects in the angry-birds-game with polygon-shape are static objects of type "hills" ) .
        

        // ASSUMPTION _> The Polygons from computervision are built anticlockwise, so we also go through the vertexlist anticlockwise 
        // log.debug("Splitting Polygon into TriangleSegments...");
        LinkedList<Vec2> vertexList = new LinkedList<Vec2>();
        for (Vec2 vertex : vertices) {
            vertexList.add(vertex);
        }
        int createdTriangles = 0;
        boolean vertexListContainsValidTriangles = true;
        while (vertexList.size() > 2 && vertexListContainsValidTriangles)
        {   
            // log.debug("vertexListSize = "+ vertexList.size());
            int triangleStartIndex  = 0;
            
            Vec2[] nextValidTriangle = null;
            while (nextValidTriangle == null){
//                if(vertexList.size() == 3 && triangleStartIndex == 0){
//                    log.debug("Stop");
//                }
                if(triangleStartIndex >= vertexList.size()){
                    log.debug("Error - could not find reasonable triangle for poly triangulation - this is a bug and should no thappen");
                    vertexListContainsValidTriangles = false;
                    break;
                }
                int triangleVertexIndex_0 = triangleStartIndex;
                int triangleVertexIndex_1 = (triangleStartIndex + 1) % vertexList.size();
                int triangleVertexIndex_2 = (triangleStartIndex + 2) % vertexList.size();
                
                if (
                    triangleVertexIndex_0 > vertexList.size() ||
                    triangleVertexIndex_1 > vertexList.size() ||
                    triangleVertexIndex_2 > vertexList.size() )
                    {
                        log.debug("ERROR : There is an Error in Polygon-Triangulation Logic  - this is a bug and should not happen");
                        break;
                    }   
                    Vec2 v0 = vertexList.get(triangleVertexIndex_0);
                    Vec2 v1 = vertexList.get(triangleVertexIndex_1);
                    Vec2 v2 = vertexList.get(triangleVertexIndex_2);
                    Vec2[] triangleVertices = new Vec2[]{v0, v1, v2};
                    // log.debug("Accessing Vertices for Triangle: " + triangleVertexIndex_0 + " , " + triangleVertexIndex_1 + " , " + triangleVertexIndex_2 + " = "  + v0.toString() + " - " + v1.toString() + " - "  + v2.toString() );
                
                
                // check if any other vertices would be inside that triangle - if so , the triangle would not make sense and we dismiss it
                boolean foundPointInsideOfTriangle = false;
                if (vertexList.size() >= 4){
                    for(int i = 0; i < vertexList.size();i++){
                        // we dont want to test containment of vertex that are themselves constituents of the triangle 
                        if ( i == triangleVertexIndex_0 || i == triangleVertexIndex_1 || i == triangleVertexIndex_2){
                            // log.debug("Skipping point: " + vertexList.get(i).toString() +" for triangle: " + v0.toString() + " - " + v1.toString()+ " - " +v2.toString()) ;
                            continue;
                        }
                        // pick the next possible vertex to be compared against the triangle

                        Vec2 testPoint = vertexList.get(i);
                        
                        foundPointInsideOfTriangle = isPointInsideOfTriangle( testPoint, triangleVertices);
                        if(foundPointInsideOfTriangle ){
                            // if the testvertex is inside the triangle
                            // log.debug("Found Point " +testPoint.toString() + " inside of the triangle "  + v0.toString() + " - " + v1.toString() + " - "  + v2.toString() );
                            break;
                        };
                     
                    }
                }

                if (foundPointInsideOfTriangle){
                    triangleStartIndex += 1;
                    continue;
                }
                
                // if the triangle is in a concave segment of the source-polygon, then it does not make sense to be there, and we can skip it
                boolean isTriangleInConcaveSegment = isPointRightOfEdgeVector(v2, new Vec2[]{v0, v1});
                if (isTriangleInConcaveSegment){
                    // log.debug("Edge  " + v0.toString() + " > " + v1.toString()  +  " is in concave segment");
                    triangleStartIndex += 1;
                    continue;
                }

                // we found a triangle that can be instanced into the world
                nextValidTriangle = triangleVertices;

                // now lets remove the middle-vertex of the triangle from the vertexList
                vertexList.remove(triangleVertexIndex_1);
            }

            // now lets add the triangle to the box2d-world
            if(nextValidTriangle!=null){
                PolygonShape shape = new PolygonShape();
                shape.set(nextValidTriangle, 3);

                BodyDef bodyDef = new BodyDef();
                bodyDef.type = BodyType.STATIC;
                bodyDef.position.set(0, 0);
                bodyDef.angle = 0;

                Body body = world.createBody(bodyDef);
                body.createFixture(shape, getProperties().getFixtureDensity());
                createdTriangles += 1;

                body.setUserData(this);

                applyEntityStateToPhysicsBody(body);
            }else{
                log.debug("Could not add triangle, it was null");
            }
            // we're done
        }
        // log.debug("Created " + createdTriangles + " Triangles out of " + vertices.length + " Vertices");


    }

    /** return the height of the the highest vertex in the polygon */
    public float getHighestVertexHeight(){
        float highest_Vertex_Height = -100000f; 
        for (Vec2 vertex : vertices) {
            highest_Vertex_Height = Math.max(highest_Vertex_Height, vertex.y);
        }
        // log.debug("Highest Vertex Height = "+ highest_Vertex_Height);
        return highest_Vertex_Height;
    }
    
    /** return the height of the the lowest vertex in the polygon */
    public float getLowestVertexHeight(){
        float lowest_Vertex_Height = 100000f; 
        for (Vec2 vertex : vertices) {
            lowest_Vertex_Height = Math.min(lowest_Vertex_Height, vertex.y);
        }
        // log.debug("Highest Vertex Height = "+ lowest_Vertex_Height);
        return lowest_Vertex_Height;
    }


    /** flip the order of the stored vertexlist */
    public void reverseVertexOrdering(){
        Vec2[] verticesReversedOrder = new Vec2[vertices.length];
        for ( int i = 0; i < vertices.length; i++){
            verticesReversedOrder[vertices.length -1 - i] = vertices[i] ;
        }
        vertices = verticesReversedOrder;
    }

    
    @Override
    public void scaleGlobally(float scalingFactor) {

        for (int i = 0; i < vertices.length; i++) {
            vertices[i].x *= scalingFactor;
            vertices[i].y *= scalingFactor;
        }
    }

    @Override
    public void mirrorOnXAxis() {
        for (Vec2 vertex : vertices) {
            vertex.y = - vertex.y;
        }
        reverseVertexOrdering();
      
    }

    @Override
    public void mirrorOnYAxis() {
        for (Vec2 vertex : vertices) {
            vertex.x = - vertex.x;
        }
        reverseVertexOrdering();
    }

    @Override
    public void translate(float x, float y) {
        super.translate(x, y);

        for (int i = 0; i < vertices.length; i++) {
            vertices[i].x += x;
            vertices[i].y += y;
        }

    }


	public Vec2[] getVertices() {
		return vertices;
	}
	



}