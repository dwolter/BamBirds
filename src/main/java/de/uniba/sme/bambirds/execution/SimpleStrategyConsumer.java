package de.uniba.sme.bambirds.execution;

import java.util.ArrayList;
import java.util.List;

import de.uniba.sme.bambirds.common.StrategyConsumer;
import de.uniba.sme.bambirds.common.objects.AbstractScene;
import de.uniba.sme.bambirds.common.objects.Node;
import de.uniba.sme.bambirds.common.objects.Plan;

public class SimpleStrategyConsumer implements StrategyConsumer {
	
	private final Node node;
	
	public SimpleStrategyConsumer(Node node) {
		this.node = node;
	}

	@Override
	public void post(List<Plan> newPlans) {
		node.createChildNodesFromTargetList(newPlans);
	}

	@Override
	public void post(Plan newPlan) {
		node.createChildNodeFromTarget(newPlan);
	}

}
