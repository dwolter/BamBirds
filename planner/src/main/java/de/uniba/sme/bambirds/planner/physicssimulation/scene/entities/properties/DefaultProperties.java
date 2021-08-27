package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

import java.io.Serializable;

import org.jbox2d.dynamics.BodyType;


/** Baseclass with default Settings from which all EntityProperites.Classes  derive from. Settings can be overwritten in the construcot of the subclasses.*/
public class DefaultProperties extends EntityProperties implements Serializable {

    private static final long serialVersionUID = 1L;

    public DefaultProperties() {
        super();
        setBodyType(BodyType.DYNAMIC);
        setMass(1.0f);// > it's  better if it is autocalculated by jbox2d based on  size + density 
        setGravityScale(1.0f);
        setAngularDamping(0.1f);
        setLinearDamping(0f);
        setFixedRotation(false);
        setAwake(true);
        setSleepingAllowed(true);
        setBullet(false);
        setFixtureFriction(1f);
        setFixtureRestitution(0.0f);
        setFixtureDensity(0.1f);
        setMaximumDamagePerMass(100f);
    }
    
}