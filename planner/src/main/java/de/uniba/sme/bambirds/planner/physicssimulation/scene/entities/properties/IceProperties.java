package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;


public class IceProperties extends DefaultProperties{

	private static final long serialVersionUID = 1L;

	public IceProperties() {
		super();
		setFixtureDensity(1f); // ice weights more than wood
		setFixtureFriction(1f);
		
	}
 
	
}