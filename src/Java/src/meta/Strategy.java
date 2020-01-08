package meta;

import java.util.List;

/**
 * The {@code Strategy} interface is used to execute (ideally concurrent)
 * implementations of different planners and receive the results.
 *
 * @see StrategyConsumer
 */
public interface Strategy {

	/**
	 * Start the {@code Strategy}'s planning. The {@code Strategy} will call
	 * {@link StrategyConsumer#post(List)} to submit its plans. The planning
	 * should finish within {@code timeOut} milliseconds, after that it may
	 * be stopped.
	 * <p>
	 * Ideally individual plans are submitted to {@code consumer} as soon as
	 * they are found and planning is stopped after {@code timeOut}.
	 *
	 * @param consumer The {@link StrategyConsumer} instance whose {@code post}
	 *                 method will be used for the callback
	 * @param timeOut  The maximum amount of time available for planning in
	 *                 milliseconds
	 */
	void plan(StrategyConsumer consumer, long timeOut);

	/**
	 * Kill the {@code Strategy}, i.e. tell it to stop planning and shut down.
	 */
	void kill();
}
