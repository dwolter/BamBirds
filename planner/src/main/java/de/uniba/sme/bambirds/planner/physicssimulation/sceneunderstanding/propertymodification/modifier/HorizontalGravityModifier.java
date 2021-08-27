package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier;

import java.util.function.BiConsumer;

import org.jbox2d.common.Vec2;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyModifier;

public class HorizontalGravityModifier implements PropertyModifier {

    @Override
    public BiConsumer<Simulation, Float> getFunction() {
        return (sim, value) -> {
            Vec2 gravity = sim.getSettings().getGravity();
            gravity.set(value.floatValue(), gravity.y);
        };
    }

    @Override
    public float getCurrentPropertyValue(Simulation simulation) {
        return simulation.getSettings().getGravity().x;
    }

    @Override
    public String toString() {
        return "Horizontal Gravity Modifier";
    }
    
    @Override
    public MODIFIER_TYPE getModifierType() {
        return PropertyModifier.MODIFIER_TYPE.HORIZONTAL_GRAVITY;
    }

}