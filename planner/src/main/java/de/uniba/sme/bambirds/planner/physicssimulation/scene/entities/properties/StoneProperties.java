package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

public class StoneProperties extends DefaultProperties {

	private static final long serialVersionUID = 1L;

	public StoneProperties() {
		super();
		setFixtureDensity(2.0f); // stones weigh more than ice and wood
		setFixtureFriction(0.25f);
	}
    
}