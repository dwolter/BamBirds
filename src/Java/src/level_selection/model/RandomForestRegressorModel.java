package level_selection.model;

import java.util.HashMap;
import java.util.Map;

import level_selection.model_representation.RandomForestRegressor;

/**
 * This Class runs the {@code model_representation.RandomForestRegressor} with the input features
 */
public class RandomForestRegressorModel implements RegressorModel {

	private Map<Integer, Map<String, Integer>> levelFeatures;

	public RandomForestRegressorModel() {
		this.levelFeatures = new HashMap<Integer, Map<String, Integer>>();
	}

	@Override
	public Map<Integer, Integer> predict(Map<Integer, Map<String, Integer>> levelFeatures) {
		// calculate results from classifier model
		this.levelFeatures.putAll(levelFeatures);
		Map<Integer, Integer> predictions = new HashMap<Integer, Integer>();

		for (Map.Entry<Integer, Map<String, Integer>> entry : this.levelFeatures.entrySet()) {
			int levelId = entry.getKey();
			Map<String, Integer> featureMap = entry.getValue();

			int[] predArr = RandomForestRegressor.predict(featureMap.get("max_score"), featureMap.get("num_birds"),
					featureMap.get("num_destroyable_objects"), featureMap.get("num_generated_shots"),
					featureMap.get("num_line_segments_hills"), featureMap.get("num_pigs"), featureMap.get("num_strategies"),
					featureMap.get("num_times_played"), featureMap.get("numerical_strategies"));

			int score = predArr[0];

			predictions.put(levelId, score);
		}

		return predictions;
	}
}
