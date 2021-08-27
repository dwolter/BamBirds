package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties;

public class PigProperties extends DefaultProperties{

    private static final long serialVersionUID = 1L;

    public PigProperties(){
        super();
        setFixtureDensity(1f);
        setFixtureFriction(1);
        setAngularDamping(5f);

        setMaximumDamagePerMass(15f);
    }
}