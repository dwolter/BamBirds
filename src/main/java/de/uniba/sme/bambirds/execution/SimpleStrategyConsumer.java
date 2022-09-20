package de.uniba.sme.bambirds.execution;

import java.util.List;

import de.uniba.sme.bambirds.common.StrategyConsumer;
import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.Plan;

public class SimpleStrategyConsumer implements StrategyConsumer {
	
	private final Node node;
	
	public SimpleStrategyConsumer(Node node) {
		this.node = node;
	}

	@Override
	public List<Node> post(List<Plan> newPlans) {
		return node.createChildNodesFromTargetList(newPlans);
	}

	@Override
	public Node post(Plan newPlan) {
		return node.createChildNodeFromTarget(newPlan);
	}

}
