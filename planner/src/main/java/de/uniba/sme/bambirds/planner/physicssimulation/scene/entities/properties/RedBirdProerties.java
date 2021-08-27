package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

public class RedBirdProerties extends DefaultProperties {

	private static final long serialVersionUID = 1L;

	public RedBirdProerties() {
		super();
		setAngularDamping(1f);
		setBullet(true);
		setFixtureDensity(3f);
		setFixtureRestitution(0.4f);
		setFixtureFriction(0.25f);
	}
    
}