package meta;

import planner.Target;

import java.util.List;

/**
 * The {@code StrategyConsumer} interface is used to receive strategies/plans
 * from {@link Strategy} implementations.
 *
 * @see Strategy
 */
public interface StrategyConsumer {

	/**
	 * Receive {@code newTargets} found by a {@code Strategy}.
	 *
	 * @param newTargets targets found by a {@code Strategy}
	 */
	void post(List<Target> newTargets);

}
