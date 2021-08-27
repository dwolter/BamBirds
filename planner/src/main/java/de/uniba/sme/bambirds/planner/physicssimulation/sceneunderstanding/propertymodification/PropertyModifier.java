package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification;

import java.util.function.BiConsumer;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;

public interface PropertyModifier {

    enum MODIFIER_TYPE {
        VERTICAL_GRAVITY,
        HORIZONTAL_GRAVITY,
        FRICTION,
        RESTITUTION,
        DENSITY,
        ANGULAR_DAMPING,
        LINEAR_DAMPING
    }

    BiConsumer<Simulation, Float> getFunction();

    float getCurrentPropertyValue(Simulation simulation);

    MODIFIER_TYPE getModifierType();

}