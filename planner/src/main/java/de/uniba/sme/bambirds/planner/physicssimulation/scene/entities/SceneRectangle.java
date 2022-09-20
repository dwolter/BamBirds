package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import org.jbox2d.collision.shapes.PolygonShape;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyDef;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import de.uniba.sme.bambirds.planner.physicssimulation.SimulationUtils;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/**Used for Objects that have rectangular form  ( e.g. Blocks of ABType ice/wood/stone and the main ground) */
public class SceneRectangle extends SceneEntityBase {
    private static final Logger log = LogManager.getLogger(SceneRectangle.class);


    /**
     * Version number for serialisation.
     */

     private static final long serialVersionUID = 1L;

    private float width = 0.0f;
    private float height = 0.0f;      

    
    /**
     * Function to Copy all State ( attribute-values ) from this entity to another entity
     * @param targetEntity
     */
    public void CopyEntityState(SceneRectangle targetEntity){
        super.CopyEntityState(targetEntity);
        targetEntity.width = width;
        targetEntity.height = height;
    }

    /**
     * This Constructor is meant as a copyconstructor. you can put in a scnerectangle and it will return a new scenerectangle with an equal state
     * @param sourceSceneRectangle
     */
    public SceneRectangle(SceneRectangle sourceSceneRectangle){
        super(sourceSceneRectangle.getAbOriginalObject());
        sourceSceneRectangle.CopyEntityState(this);
    }


    /**
     * This Constructor is meant for creating a Scenerectangle-Entity out of the computervisions inputdata ( a Rect )
     * @param rect
     */
    public SceneRectangle(Rect rect) {
        super(rect);

        width  = (float) rect.getPWidth();
        height = (float) rect.getPLength();
    
    }

    /**
     * This Constructor is meant for creating a Scenerectangle-Entity out of the computervisions inputdata ( a Rect )
     * @param rect
     */
    public SceneRectangle(ABObject rect) {
        super(rect);

        width  = (float) rect.getWidth();
        height = (float) rect.getHeight() ;

    }

    public static SceneRectangle createSceneRectangle(String globalID, int id, ABType abType, float width, float height, float positionX, float positionY ,float angle){
        Rect rect = new Rect(positionX, positionY, width,height, angle, abType);
        SceneRectangle sceneRectangle = new SceneRectangle(rect);
        sceneRectangle.setGlobalID(globalID);
        sceneRectangle.setId(id);
        return sceneRectangle;
    }


    @Override
    public void addToWorld(World world) {
        PolygonShape shape = new PolygonShape();
        // jbox2d expecteds halfWidth/ halfHeight
        shape.setAsBox((float) width / 2f, (float) height / 2f);

        BodyDef bodyDef = new BodyDef();
        bodyDef.type = getProperties().getBodyType();
        bodyDef.position.set(getCenterX(), getCenterY());
        bodyDef.angle = getAngle();
        bodyDef.allowSleep = getProperties().isSleepingAllowed();
        
        Body body = world.createBody(bodyDef);
        body.createFixture(shape, getProperties().getFixtureDensity());
        body.setUserData(this);

        applyEntityStateToPhysicsBody(body);

    }

    @Override
    public void scaleGlobally(float scalingFactor) {
        super.scaleGlobally(scalingFactor);

        height *= scalingFactor;
        width *= scalingFactor;
    }

    @Override
    public void scaleLocally(float scalingFactor) {
        super.scaleLocally(scalingFactor);

        height *= scalingFactor;
        width *= scalingFactor;
    }

    public float getWidth() {
        return width;
    }

    public void setWidth(float Width) {
        this.width = Width;
    }

    public float getHeight() {
        return height;
    }

    public void setHeight(float Height) {
        this.height = Height;
    }

    @Override
    public String toString() {
        String s = super.toString();
        s += SimulationUtils.padRight(  " | Width: "  + width ,  25)
        + SimulationUtils.padRight(  " | Height: " + height  ,  25); 
        return s;
    }

}