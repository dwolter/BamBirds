package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.manipulation;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;

/**
 * Just an example template used for report
 */
public class ExampleWorldModifier implements IWorldModifier {

    @Override
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException {
        //Do Stuff
    }

    @Override
    public String getName() {
        return "ExampleWorldModifier";
    }
    
}