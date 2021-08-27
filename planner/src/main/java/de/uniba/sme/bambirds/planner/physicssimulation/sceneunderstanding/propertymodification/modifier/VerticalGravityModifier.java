package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier;

import java.util.function.BiConsumer;

import org.jbox2d.common.Vec2;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyModifier;

public class VerticalGravityModifier implements PropertyModifier {
    
    @Override
    public BiConsumer<Simulation, Float> getFunction() {
        return (sim, value) -> {
            Vec2 gravity = sim.getSettings().getGravity();
            gravity.set(gravity.x, value.floatValue());
        };
    }

    @Override
    public float getCurrentPropertyValue(Simulation simulation) {
        return simulation.getSettings().getGravity().y;
    }

    @Override
    public String toString() {
        return "Vertical Gravity Modifier";
    }
    
    @Override
    public MODIFIER_TYPE getModifierType() {
        return PropertyModifier.MODIFIER_TYPE.VERTICAL_GRAVITY;
    }

}