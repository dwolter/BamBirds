package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier;

import java.util.function.BiConsumer;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyModifier;

/**
 * Does nothing, may used as modifer in ModificationTree Root node
 */
public class DummyModifier implements PropertyModifier {

    @Override
    public BiConsumer<Simulation, Float> getFunction() {
        //Do nothing
        return (a, b) -> {};
    }

    @Override
    public float getCurrentPropertyValue(Simulation simulation) {
        return 0;
    }

    @Override
    public String toString() {
        return "Dummy Modifier >> No modification";
    }

    @Override
    public MODIFIER_TYPE getModifierType() {
        return null;
    }
    
}