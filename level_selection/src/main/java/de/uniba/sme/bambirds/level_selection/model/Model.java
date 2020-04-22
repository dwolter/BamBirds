package de.uniba.sme.bambirds.level_selection.model;

import java.util.Map;

/**
 * The {@code Model} interface is used to execute implementations of different
 * models and receive the results.
 *
 */
public interface Model {
	/**
	 * Start the {@link Model}`s prediction.
	 *
	 * @param levelFeatures The features to predict on
	 * @return The predictions
	 */
	Map<Integer, ? extends Number> predict(Map<Integer, Map<String, Integer>> levelFeatures);
}
