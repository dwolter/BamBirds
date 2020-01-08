package level_selection.model;

import java.util.HashMap;
import java.util.Map;

import level_selection.model_representation.RandomForestClassifier;

/**
 * This Class runs the {@code model_representation.RandomForestClassifier} with the input features
 */
public class RandomForestClassifierModel implements ClassifierModel {
	private final static short CLASS_LOST_INDEX = 0;
	private final static short CLASS_WON_INDEX = 1;

	private Map<Integer, Map<String, Integer>> levelFeatures;

	public RandomForestClassifierModel() {
		this.levelFeatures = new HashMap<Integer, Map<String, Integer>>();
	}

	@Override
	public Map<Integer, Double> predict(Map<Integer, Map<String, Integer>> levelFeatures) {
		// calculate results from classifier model
		this.levelFeatures.putAll(levelFeatures);
		Map<Integer, Double> predictions = new HashMap<Integer, Double>();

		for (Map.Entry<Integer, Map<String, Integer>> entry : this.levelFeatures.entrySet()) {
			int levelId = entry.getKey();
			Map<String, Integer> featureMap = entry.getValue();

			int[] predArr = RandomForestClassifier.predict(featureMap.get("max_score"), featureMap.get("num_birds"),
					featureMap.get("num_destroyable_objects"), featureMap.get("num_generated_shots"),
					featureMap.get("num_line_segments_hills"), featureMap.get("num_pigs"), featureMap.get("num_strategies"),
					featureMap.get("num_times_played"), featureMap.get("numerical_strategies"));

			int lostAmount = predArr[CLASS_LOST_INDEX];
			int wonAmount = predArr[CLASS_WON_INDEX];

			double probWin = (double) wonAmount / (wonAmount + lostAmount);

			predictions.put(levelId, probWin);
		}

		return predictions;
	}
}
