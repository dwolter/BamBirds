package de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.modificationtree;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.sceneunderstanding.propertymodification.PropertyState;

public class ModificationTreeNode {

	private ModificationTreeNode parent;
	private PropertyState state;
	private float value;

	private List<ModificationTreeNode> children;

	public ModificationTreeNode(PropertyState state, float value, ModificationTreeNode parent) {
		this.state = state;
		this.value = value;
		this.parent = parent;

		children = new LinkedList<>();
	}

	public void addChild(PropertyState state, float distance) {
		ModificationTreeNode newNode = new ModificationTreeNode(state, distance, this);
		children.add(newNode);
	}

	public void addChild(ModificationTreeNode node) {
		children.add(node);
	}

	public void applyTo(Simulation simulation) {
		state.applyTo(simulation);
	}

	public void applyEntireBranch(Simulation simulation) {
		LinkedList<ModificationTreeNode> nodeStack = new LinkedList<>();
		nodeStack.add(this);
		ModificationTreeNode parentNode = parent;
		while (parentNode != null) {
			//preserve application order
			nodeStack.addFirst(parentNode);
			parentNode = parentNode.parent;
		}
		for (ModificationTreeNode node : nodeStack) {
			node.applyTo(simulation);
		}
	}

	public ModificationTreeNode getChildWithLowestValue() {
		ModificationTreeNode minDistanceNode = children.stream()
				.min(Comparator.comparing(ModificationTreeNode::getValue)).orElse(null);
		return minDistanceNode;
	}

	public LinkedList<ModificationTreeNode> printBranchModificationStack() {
		LinkedList<ModificationTreeNode> nodeStack = new LinkedList<>();
		nodeStack.add(this);
		ModificationTreeNode parentNode = parent;
		while (parentNode != null) {
			//preserve application order
			nodeStack.addFirst(parentNode);
			parentNode = parentNode.parent;
		}
		System.out.println("Modifiaction Stack: ");
		for (ModificationTreeNode node : nodeStack) {
			System.out.println("\t" + node.getPropertyState());
		}
		return nodeStack;
	}

	public void setValue(float value) {
		this.value = value;
	}

	public float getValue() {
		return value;
	}

	public PropertyState getPropertyState() {
		return state;
	}

	public ModificationTreeNode getParent() {
		return parent;
	}

	public List<ModificationTreeNode> getChildren() {
		return children;
	}
}