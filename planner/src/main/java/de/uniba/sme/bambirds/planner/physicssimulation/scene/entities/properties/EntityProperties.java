package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

import org.jbox2d.collision.shapes.MassData;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.Fixture;

import java.io.Serializable;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;



/**A Setup-Object containing all the physics-properties which may be specific for a certain ABType - e.g  . wood-objects have odifferent density and friction than stone-objects or ice-objects. Subclasses for each object type exist, so that each type can be tweaked individually by overwriting properties of their parent-class. All subclasses share a Baseclass "DefaultProperties" which has some reasonable default-property-values already set up. The Properties for each object will get applied automatically at the creation of a scene. We Convencience-Methods for copying and pasting all settings easily. */
public abstract class EntityProperties implements Serializable {

    private static final long serialVersionUID = 1L;

    private static final Logger log = LogManager.getLogger(EntityProperties.class);

    private BodyType bodyType;

    private float    mass;
    //private MassData massData;
    private float   gravityScale;

    private float   angularDamping;
    private float   linearDamping;

    private boolean isFixedRotation;

    private boolean isAwake;
    private boolean isSleepingAllowed;
    private boolean isBullet;
    
    private float fixtureFriction;
    private float fixtureRestitution;
    private float fixtureDensity;

    private float maximumDamagePerMass ;


    /***
     * We can copy the current values of all desired properties from a jbox2d-Body and store them in this' objects properties
     * @param body An instance of a jbox2d-body
     */
    public void copyValuesFromBody(Body body) {
        bodyType = body.getType();
        mass = body.getMass();
        //??
        //body.getMassData(massData);
        angularDamping = body.getAngularDamping();
        linearDamping = body.getLinearDamping();
        isFixedRotation = body.isFixedRotation();

        isAwake = body.isAwake();
        isSleepingAllowed = body.isSleepingAllowed();
        isBullet = body.isBullet();

        Fixture fixture = body.getFixtureList();
        // WE EXPECT AT MOST 1 !! FIXTURE to be attached to each body ( so do not create bodies with more than 1 fixture for the simulation)
        if( fixture != null){
            fixtureDensity = fixture.getDensity();
            fixtureFriction = fixture.getFriction();
            fixtureRestitution = fixture.getRestitution();
        }
    }

    

    /***
     *  We can copy the current values of all desired properties from this' objects properties and apply them to an instance of a jbox2d-body
     * @param body the target jbox2d-body
     */
    public void applyValuesToBody(Body body) {
        MassData massData = new MassData();
        massData.mass = mass;
        body.setMassData(massData);
        body.setGravityScale(gravityScale);

        body.setAngularDamping(angularDamping);
        body.setLinearDamping(linearDamping);
        body.setFixedRotation(isFixedRotation);

        body.setAwake(isAwake);
        body.setSleepingAllowed(isSleepingAllowed);
        body.setBullet(isBullet);

        //Assuming only one fixture per body
        body.getFixtureList().setDensity(fixtureDensity);
        body.getFixtureList().setRestitution(fixtureRestitution);
        body.getFixtureList().setFriction(fixtureFriction);
    }



    /***
     * We can copy all values from this object and apply them to another EnntityProperties-Object
     * @param properties the targetobject
     */
    public void copyValuesTo(EntityProperties properties) {
        properties.setMass(mass);
        properties.setGravityScale(gravityScale);
        
        properties.setAngularDamping(angularDamping);
        properties.setLinearDamping(linearDamping);
        properties.setFixedRotation(isFixedRotation);

        properties.setAwake(isAwake);
        properties.setSleepingAllowed(isSleepingAllowed);
        properties.setBullet(isBullet);

        properties.setFixtureDensity(fixtureDensity);
        properties.setFixtureFriction(fixtureFriction);
        properties.setFixtureRestitution(fixtureRestitution);

        properties.setMaximumDamagePerMass(maximumDamagePerMass);
    }


    /***
     * We can copy all values from a sourceobject and apply them to this object
     * @param properties the sourceobject
     */

    public void copyValuesFrom(EntityProperties properties) {
        mass = properties.getMass();
        gravityScale = properties.getGravityScale();

        angularDamping = properties.getAngularDamping();
        linearDamping = properties.getLinearDamping();
        isFixedRotation = properties.isFixedRotation();

        isAwake = properties.isAwake();
        isSleepingAllowed = properties.isSleepingAllowed();
        isBullet = properties.isBullet();

        fixtureDensity = properties.getFixtureDensity();
        fixtureFriction = properties.getFixtureFriction();
        fixtureRestitution = properties.getFixtureRestitution();

        maximumDamagePerMass = properties.getMaximumDamagePerMass();
    }

    
    //-------------- Getters and Setters -----------------------------------//


    public BodyType getBodyType() {

        return bodyType;
    }

    public void setBodyType(BodyType bodyType) {
        this.bodyType = bodyType;
    }

    public float getMass() {
        return mass;
    }

    public void setMass(float mass) {
        this.mass = mass;
    }

    public float getGravityScale() {
        return gravityScale;
    }

    public void setGravityScale(float gravityScale) {
        this.gravityScale = gravityScale;
    }

    public float getAngularDamping() {
        return angularDamping;
    }

    public void setAngularDamping(float angularDamping) {
        this.angularDamping = angularDamping;
    }

    public boolean isFixedRotation() {
        return isFixedRotation;
    }

    public void setFixedRotation(boolean isFixedRotation) {
        this.isFixedRotation = isFixedRotation;
    }

    public boolean isAwake() {
        return isAwake;
    }

    public void setAwake(boolean isAwake) {
        this.isAwake = isAwake;
    }

    public boolean isSleepingAllowed() {
        return isSleepingAllowed;
    }

    public void setSleepingAllowed(boolean isSleepingAllowed) {
        this.isSleepingAllowed = isSleepingAllowed;
    }

    public boolean isBullet() {
        return isBullet;
    }

    public void setBullet(boolean isBullet) {
        this.isBullet = isBullet;
    }

    public float getFixtureFriction() {
        return fixtureFriction;
    }

    public void setFixtureFriction(float fixtureFriction) {
        this.fixtureFriction = fixtureFriction;
    }

    public float getFixtureRestitution() {
        return fixtureRestitution;
    }

    public void setFixtureRestitution(float fixtureRestitution) {
        this.fixtureRestitution = fixtureRestitution;
    }

    public float getFixtureDensity() {
        return fixtureDensity;
    }

    public void setFixtureDensity(float fixtureDensity) {
        this.fixtureDensity = fixtureDensity;
    }

	public float getLinearDamping() {
		return linearDamping;
	}

	public void setLinearDamping(float linearDamping) {
		this.linearDamping = linearDamping;
	}



	@Override
	public String toString() {
		return "angularDamping=" + angularDamping + ", bodyType=" + bodyType + ", fixtureDensity="
				+ fixtureDensity + ", fixtureFriction=" + fixtureFriction + ", fixtureRestitution=" + fixtureRestitution
				+ ", gravityScale=" + gravityScale + ", isAwake=" + isAwake + ", isBullet=" + isBullet
				+ ", isFixedRotation=" + isFixedRotation + ", isSleepingAllowed=" + isSleepingAllowed
				+ ", linearDamping=" + linearDamping + ", mass=" + mass ;
	}



	public float getMaximumDamagePerMass() {
		return maximumDamagePerMass;
	}



	public void setMaximumDamagePerMass(float maximumDamagePerArea) {
		this.maximumDamagePerMass = maximumDamagePerArea;
	}

}