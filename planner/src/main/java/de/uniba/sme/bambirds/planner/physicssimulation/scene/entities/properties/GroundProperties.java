package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

import org.jbox2d.dynamics.BodyType;

public class GroundProperties extends DefaultProperties{

	private static final long serialVersionUID = 1L;

	public GroundProperties() {
		super();
		setBodyType(BodyType.STATIC);
		setFixtureFriction(1f);
		setFixtureRestitution(0.1f);
	}
    
}