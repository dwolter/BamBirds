package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.modificationtree;

import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyState;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.modifier.DummyModifier;

public class ModificationTree {
    
    private ModificationTreeNode root;

    public ModificationTree () {
        PropertyState state = new PropertyState(new DummyModifier(), 0);
        root = new ModificationTreeNode(state, 0f, null);
    }

    public ModificationTreeNode getRoot() {
        return root;
    }

    public void setRootValue(float value) {
        root.setValue(value);
    }

    public float getRootValue() {
        return root.getValue();
    }

    /**
     * Bla TODO whatever
     */


}