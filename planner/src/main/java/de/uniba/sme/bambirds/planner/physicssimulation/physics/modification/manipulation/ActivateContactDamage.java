package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.manipulation;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.contactListener.AngryBirdsContactListener;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.contactListener.DefaultContactListener;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;

import org.apache.log4j.*;

/**
 * Activates damges via contact in the simulation by adding contact listeners the the repective objects
 */
public class ActivateContactDamage implements IWorldModifier{

    private static final Logger log = LogManager.getLogger(ActivateContactDamage.class);
	boolean setActive = false;


   public ActivateContactDamage(boolean setActive){
		this.setActive = setActive;
   }

	@Override
	public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException {

		if(setActive){
            world.setContactListener(new AngryBirdsContactListener());
		}else{
            world.setContactListener(new DefaultContactListener());
		}
	}

	@Override
	public String getName() {
		return "ActivateContactDamage("+setActive+")";
    }
    
    // biggger mass > bigger impacts > more likely to pass destruction threshold on other objects or self ( when falling down )
    // only apply impulse if has not been touched before by impact-source-object

  
    
}
