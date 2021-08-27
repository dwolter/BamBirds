package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.manipulation;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;



/** 
 * You can use this as a placeholder for some WorldModifier. 
 * When its called, it will log a message to the console, so that you know that it works 
 */
public class DummySimualationManipulator implements IWorldModifier {
    private static final Logger log = LogManager.getLogger(DummySimualationManipulator.class);

    @Override
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings) {
        log.debug("I'm just a Dummy Manipulator, I actually dont do anything");
    }

    @Override
    public String getName() {
        return "DummyManipulator";
    }
    
}