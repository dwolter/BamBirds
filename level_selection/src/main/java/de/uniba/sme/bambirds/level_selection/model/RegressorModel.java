package de.uniba.sme.bambirds.level_selection.model;

import java.util.Map;

/**
 * The {@code RegressorModel} interface is used to execute implementations of different
 * regressor models and return the results.
 *
 */
public interface RegressorModel extends Model {
	
	Map<Integer, Integer> predict(Map<Integer, Map<String, Integer>> levelFeatures);
}
