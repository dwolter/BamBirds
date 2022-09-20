package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities;

import org.jbox2d.collision.shapes.CircleShape;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyDef;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;


/**Scene-Circle is meant to be used for all round objects, which can be defined by a centerposition and a radius, i.e. mainly for birds, pigs, and  rounded-stones  */
public class SceneCircle extends SceneEntityBase {
    private static final Logger log = LogManager.getLogger(SceneCircle.class);


    private static final long serialVersionUID = 1L;
    
    private Circle originalCircle;

    private float radius;
   

     /**
     * Function to Copy all State ( attribute-values ) from this entity to another entity
     * @param targetEntity
     */

    public void CopyEntityState(SceneCircle targetEntity){
        super.CopyEntityState(targetEntity);
        targetEntity.originalCircle = originalCircle;
        targetEntity.radius = radius;
    }

    /**
     * This Constructor is meant as a copyconstructor. you can put in a SceneCircle and it will return a new SceneCircle with an equal state
     * @param sourceSceneRectangle
     */
    public SceneCircle(SceneCircle sourceSceneCircle){
        super(sourceSceneCircle.originalCircle);
        sourceSceneCircle.CopyEntityState(this);
    }

    /**
     * This Constructor is meant for creating a Scenecircle-Entity out of a BodyObject of type Circle from the computervisions inputdata
     * @param circle
     */
    public SceneCircle(Circle circle) {
        super(circle);
        this.originalCircle = circle;
        radius = (float) circle.getRadius();
    }


    /**
     * This is a convenienceMethod for creating a Circle on the fly, e.g when you are creating your own Scenes, altering existing Scenes etc. ( For example you could spawn a new bird ).
     */
    public static SceneCircle createSceneCircle(String globalID, int id, ABType abType, float radius, float positionX, float positionY){
        Circle circle = new Circle(positionX, positionY, radius, abType);
        SceneCircle sceneCircle = new SceneCircle(circle);
        sceneCircle.setGlobalID(globalID);
        sceneCircle.setId(id);
        return sceneCircle;
    }
    


    @Override
    public void addToWorld(World world) {
        CircleShape shape = new CircleShape();
        shape.setRadius(radius);


        BodyDef bodyDef = new BodyDef();
        bodyDef.type = getProperties().getBodyType();
        bodyDef.position.set(getCenterX(), getCenterY());
        // bodyDef.angle = getAngle();
        bodyDef.allowSleep = getProperties().isSleepingAllowed();

        Body body = world.createBody(bodyDef);
        body.createFixture(shape, getProperties().getFixtureDensity());
        body.setUserData(this);
        
        applyEntityStateToPhysicsBody(body);
    }

    @Override
    public void scaleGlobally(float scalingFactor) {
        super.scaleGlobally(scalingFactor);
        radius *= scalingFactor;
    }

    @Override
    public void scaleLocally(float scalingFactor) {
        radius *= scalingFactor;
    }
    
    public float getRadius() {
        return radius;
    }

    public void setRadius(float radius) {
        this.radius = radius;
    }

    public Circle getOriginalCircle() {
        return originalCircle;
    }

    @Override
    public String toString() {
        String s = super.toString();

        s +=   " | Radius: " + radius;
        return s;
    }

}