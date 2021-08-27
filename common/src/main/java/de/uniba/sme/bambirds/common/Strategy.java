package de.uniba.sme.bambirds.common;

import java.util.List;

import de.uniba.sme.bambirds.common.objects.Plan;

/**
 * The {@code Strategy} interface is used to execute (ideally concurrent)
 * implementations of different planners and receive the results.
 *
 * @see StrategyConsumer
 */
public interface Strategy {

	/**
	 * Start the {@code Strategy}'s planning. The {@code Strategy} will call
	 * {@link StrategyConsumer#post(List)} or {@link StrategyConsumer#post(Plan)}
	 * to submit its plans. The planning should finish within {@code timeOut}
	 * milliseconds, after that it may be stopped.
	 * <p>
	 * Ideally individual plans are submitted to {@code consumer} as soon as they
	 * are found and planning is stopped after {@code timeOut}.
	 *
	 * @param consumer The {@link StrategyConsumer} instance whose {@code post}
	 *                 method will be used for the callback
	 * @param timeOut  The maximum amount of time available for planning in
	 *                 milliseconds
	 */
	void plan(StrategyConsumer consumer, long timeOut);
	


	/**
	 * Start the {@code Strategy}'s planning. The {@code Strategy} will return all found Targets. 
	 * The planning should finish within {@code timeOut} milliseconds, after that it will
	 * be stopped and if the Strategy supports it unfiltered plans are then returned.
	 * 
	 * @param timeOut  The maximum amount of time available for planning in
	 *                 milliseconds
	 */
	List<Plan> planSynchronously(long timeOut);

	/**
	 * Kill the {@code Strategy}, i.e. tell it to stop planning and shut down.
	 */
	void kill();

	/**
	 * Returns {@code true} if this strategy was cancelled before it completed normally.
	 * @return {@code true} if this strategy was cancelled before it completed
	 */
	boolean isCancelled();
}
