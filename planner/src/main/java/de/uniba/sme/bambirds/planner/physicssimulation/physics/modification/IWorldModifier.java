package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;


/**
 * Apply arbitrary state changes to a world.
 */
public interface IWorldModifier {
    
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException;

    public String getName();
    
}