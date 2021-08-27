package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

public class WoodProperties extends DefaultProperties{

    private static final long serialVersionUID = 1L;

	public WoodProperties() {
		super();
		setFixtureDensity(1.0f);
		setFixtureFriction(1f);
	}
    
}