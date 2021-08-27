package de.uniba.sme.bambirds.common;

import de.uniba.sme.bambirds.common.objects.Plan;

import java.util.List;

/**
 * The {@code StrategyConsumer} interface is used to receive strategies/plans
 * from {@link Strategy} implementations.
 *
 * @see Strategy
 */
public interface StrategyConsumer {

	/**
	 * Receive new {@link Plan}s found by a {@link Strategy}.
	 *
	 * @param newPlans targets found by a {@link Strategy}
	 */
	void post(List<Plan> newPlans);
	
	/**
	 * Receive a new {@link Plan} found by a {@link Strategy}.
	 *
	 * @param newPlan found by a {@link Strategy}
	 */
	void post(Plan newPlan);

}
