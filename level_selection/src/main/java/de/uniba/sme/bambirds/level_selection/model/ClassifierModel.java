package de.uniba.sme.bambirds.level_selection.model;

import java.util.Map;

/**
 * The {@code ClassifierModel} interface is used to execute implementations of different
 * classifier models and return the results.
 *
 */
public interface ClassifierModel extends Model {
	
	Map<Integer, Double> predict(Map<Integer, Map<String, Integer>> levelFeatures);
}
