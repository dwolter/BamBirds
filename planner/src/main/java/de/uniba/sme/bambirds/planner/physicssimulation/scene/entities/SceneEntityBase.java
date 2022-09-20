package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities;

import java.io.Serializable;
import java.util.HashSet;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.SimulationUtils;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties.DefaultProperties;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties.EntityProperties;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/**Base class for Sceneentities already preimplementing some of the required methods. Deriving classes may need to reimplement some of the methods ( e.g. scaling or rotating ) depending on the shape in order to behave correctly.
 A SceneEntity stores a reference to the original Computer-Vision-Body from which it also derives some of its initial static state ( id, mbrSize, position, angle...).
 Whenever we create a Snapshot out of a running world, some of the entities state will be dynamically updated ( position, angle, velocities).
 The EntityProperties define the entities physical behavior once it is instanced as a body in a world. They are derived from the ABType of the entity (e.g wood, stone, ice etc.).
*/
public abstract class SceneEntityBase implements SceneEntity, Serializable {
    private static final Logger log = LogManager.getLogger(SceneEntityBase.class);



    private static final long serialVersionUID = 1L;

    
    private ABObject abOriginalObject; // this is the only referencefield that is not "copied" when applying a copy of this class. Instead a Reference is passed

    private String globalID;
    private int id;

    private ABType abType;

    private float mbrWidth;
    private float mbrHeight;
    
    private float positionCenterX;
    private float positionCenterY;
    private float angle;

    private float linearVelocityX;
    private float linearVelocityY;
    private float angularVelocity;
    
    private EntityProperties properties;

    private HashSet<String> inContactWith = new HashSet<>();
    private float damage = 0f;

    
    /**
     * This Constructor is meant for creating a SceneEntity out of a Body from the computervisions inputdata
     * @param abShapeBody
     */
    public SceneEntityBase(ABObject abShapeBody) {
        
        globalID = abShapeBody.getGlobalID();
        id = abShapeBody.getId();
        abOriginalObject = abShapeBody;
        positionCenterX = (float) abShapeBody.getCenterX();
        positionCenterY = (float) abShapeBody.getCenterY();
        
        mbrHeight = (float) abShapeBody.getHeight();
        mbrWidth = (float) abShapeBody.getWidth();
        
        angle = (float) abShapeBody.getAngle() - 0.5f * (float) Math.PI;
        
        abType = abShapeBody.getType();
        
        properties = new DefaultProperties();
    }

    public HashSet<String> getInContactWith() {
		return inContactWith;
	}

	public void setInContactWith(HashSet<String> inContactWith) {
		this.inContactWith = inContactWith;
	}

	/**
     * Function to easily copy the state ( attribute-values ) from this entity to another entity
     * @param targetEntity
     */
    public void CopyEntityState(SceneEntityBase targetEntity){
            targetEntity.globalID = globalID; 
            targetEntity.id = id; 
            targetEntity.abOriginalObject = abOriginalObject;
            targetEntity.mbrWidth = mbrWidth;
            targetEntity.mbrHeight = mbrHeight;
            targetEntity.abType = abType;
            targetEntity.positionCenterX = positionCenterX;
            targetEntity.positionCenterY = positionCenterY;
            targetEntity.angle = angle;

            targetEntity.angularVelocity = angularVelocity;
            targetEntity.linearVelocityX = linearVelocityX;
            targetEntity.linearVelocityY = linearVelocityY;


            targetEntity.properties.copyValuesFrom(properties);
    }


    /**  Update the scene-entity with all relevant data from the instanced body 
     * @param sourceJBox2DBody the body-object whose state will be read and copied
    */
    public void storeStateFromPhysicsBodySnapshot(org.jbox2d.dynamics.Body sourceJBox2DBody){

        angle = sourceJBox2DBody.getAngle();
        positionCenterX = sourceJBox2DBody.getPosition().x;
        positionCenterY = sourceJBox2DBody.getPosition().y;

        linearVelocityX = sourceJBox2DBody.getLinearVelocity().x;
        linearVelocityY = sourceJBox2DBody.getLinearVelocity().y;

        angularVelocity = sourceJBox2DBody.getAngularVelocity();

        properties.copyValuesFromBody(sourceJBox2DBody);
    }


    /**
     * Update a Physics-Body with by applying the scene-entites state to it.
     * @param targetJBox2DBody the body-object 
     */
    public void applyEntityStateToPhysicsBody(org.jbox2d.dynamics.Body targetJBox2DBody) {
        // First lets apply dynamic values 
        targetJBox2DBody.setLinearVelocity(new Vec2(linearVelocityX, linearVelocityY));
        targetJBox2DBody.setAngularVelocity(angularVelocity);
        // Then lets apply static object-specific values 
        properties.applyValuesToBody(targetJBox2DBody);
    }

    @Override
    public abstract void addToWorld(World world);

    @Override
    public String toString(){
        String s = "SimEntity: "
        + SimulationUtils.padRight( ": | ID= " + id + " / " + globalID  , 25)
        + SimulationUtils.padRight(  " | ABShape: " + abOriginalObject.getShape(), 18)
        + SimulationUtils.padRight(  " | ABType: " +  abType  , 18)
        + " | MBR width/height : " + "( " + SimulationUtils.padLeft(String.valueOf(mbrWidth) ,  5)  + " | " +  SimulationUtils.padRight(String.valueOf(mbrHeight) ,  5)  + " )  "
        + SimulationUtils.padRight( "| Center: ( " + positionCenterX + " | " + positionCenterY + " )", 30)
        + SimulationUtils.padRight(" | Angle: " + abOriginalObject.getAngle(),  30);
        
        return s;
    }
    
    public void scaleGlobally(float scalingFactor) {
        positionCenterX *= scalingFactor;
        positionCenterY *= scalingFactor;
        mbrHeight *= scalingFactor;
        mbrWidth *= scalingFactor;
    }

    public void scaleLocally(float scalingFactor) {
        mbrHeight *= scalingFactor;
        mbrWidth *= scalingFactor;
    }

    public void setCenterPosition(float x, float y) {
        positionCenterX = x;
        positionCenterY = y;
    }

    public void translate(float x, float y) {
        positionCenterX += x;
        positionCenterY += y;
    }

    public void rotateLocally(float angle) {
        this.angle += angle;
    }

    public void setLocalRotation(float angle) {
        setAngle(angle);
    }

    public void mirrorOnXAxis() {
        positionCenterY *= -1.0f;
        angle *= -1.0f;
    }

    public void mirrorOnYAxis() {
        positionCenterX *= -1.0f;
        angle = ((float) Math.PI) - angle;
    }

    public String getGlobalID() {
        return globalID;
    }

    public void setGlobalID(String globalID) {
        this.globalID = globalID;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public float getCenterX() {
        return positionCenterX;
    }

    public void setCenterX(float centerX) {
        this.positionCenterX = centerX;
    }

    public float getCenterY() {
        return positionCenterY;
    }

    public void setCenterY(float centerY) {
        this.positionCenterY = centerY;
    }

    public float getMbrWidth() {
        return mbrWidth;
    }

    public void setMbrWidth(float mbrWidth) {
        this.mbrWidth = mbrWidth;
    }

    public float getMbrHeight() {
        return mbrHeight;
    }

    public void setMbrHeight(float mbrHeight) {
        this.mbrHeight = mbrHeight;
    }

    public float getAngle() {
        return angle;
    }

    public void setAngle(float angle) {
        this.angle = angle;
    }

    public ABType getAbType() {
        return abType;
    }

    public void setAbType(ABType abType) {
        this.abType = abType;
    }

    public EntityProperties getProperties() {
        return properties;
    }

    public void setProperties(EntityProperties properties) {
        this.properties = properties;
    }

    public float getLinearVelocityX() {
        return linearVelocityX;
    }

    public void setLinearVelocityX(float linearVelocityX) {
        this.linearVelocityX = linearVelocityX;
    }

    public float getLinearVelocityY() {
        return linearVelocityY;
    }

    public void setLinearVelocityY(float linearVelocityY) {
        this.linearVelocityY = linearVelocityY;
    }

    public float getAngularVelocity() {
        return angularVelocity;
    }

    public void setAngularVelocity(float angularVelocity) {
        this.angularVelocity = angularVelocity;
    }

	public ABObject getAbOriginalObject() {
		return abOriginalObject;
	}

	@Override
	public void addDamage(float additionalDamage) {
        damage += additionalDamage;
        // damage = Math.min(damage, 1f);
	}

	@Override
	public float getTotalDamage() {
		return damage;
	}
    
}
