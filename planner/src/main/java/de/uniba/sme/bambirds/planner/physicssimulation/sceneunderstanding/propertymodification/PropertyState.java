package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;

public class PropertyState {
    
    private float value;
    private PropertyModifier modifier;

    public PropertyState(PropertyModifier modifier, float value) {
        this.modifier = modifier;
        this.value = value;
    }

    public void applyTo(Simulation simulation) {
        modifier.getFunction().accept(simulation, value);
    }

    public float getValue() {
        return value;
    }

    public PropertyModifier getModifier() {
        return modifier;
    }

    public String toString() {
        return "(" + modifier.toString() + ": " + value + ")";
    }

}