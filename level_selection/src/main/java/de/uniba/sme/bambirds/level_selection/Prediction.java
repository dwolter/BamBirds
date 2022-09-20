package de.uniba.sme.bambirds.level_selection;

import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.database.LevelStorage;
import de.uniba.sme.bambirds.level_selection.model.ClassifierModel;
import de.uniba.sme.bambirds.level_selection.model.DecisionTreeClassifierModel;
import de.uniba.sme.bambirds.level_selection.model.DecisionTreeRegressorModel;
import de.uniba.sme.bambirds.level_selection.model.LinearRegressorModel;
import de.uniba.sme.bambirds.level_selection.model.RandomForestClassifierModel;
import de.uniba.sme.bambirds.level_selection.model.RandomForestRegressorModel;
import de.uniba.sme.bambirds.level_selection.model.RegressorModel;
import de.uniba.sme.bambirds.common.utils.Settings;

/**
 * Wrapper class for different score and win predictions
 */
public class Prediction {

	public enum ClassifierType {
		DECISION_TREE, RANDOM_FOREST
	}

	public enum RegressorType {
		DECISION_TREE, LINEAR_MODEL, RANDOM_FOREST
	}

	public static final Map<String, Integer> STRATEGY_WEIGHTS;
	static {
		Hashtable<String, Integer> tmp = new Hashtable<String, Integer>(6);
		// all the known strategies with the weights used in our decision tree in python
		tmp.put("bunker", 1);
		tmp.put("domino", 2);
		tmp.put("collapseStructure", 4);
		tmp.put("heavyObject", 10);
		tmp.put("targetPig", 5);
		tmp.put("defrost", 15);
		STRATEGY_WEIGHTS = Collections.unmodifiableMap(tmp);
	}

	private int levelRange;

	// key: levelId, value: the feature map of the level
	private Map<Integer, Map<String, Integer>> levelFeatures;

	private ClassifierModel classifierModel;

	private RegressorModel regressorModel;

	private Map<Integer, PredictionTuple<Integer, Double>> predictions;

	/**
	 * Construct a Prediction instance that handles the execution of the selected Models
	 * 
	 * @param levelRange The range of levels we want to get predictions for
	 * @param classifierType The selected Classifier
	 * @param regressorType The selected Regressor
	 */
	public Prediction(int levelRange, ClassifierType classifierType, RegressorType regressorType) {
		this.levelRange = levelRange; // e.g. 21, i.e. loop from 1 - 21 (inclusive)
		this.levelFeatures = new HashMap<Integer, Map<String, Integer>>(levelRange);
		this.predictions = new HashMap<Integer, PredictionTuple<Integer, Double>>();

		switch (classifierType) {
		case RANDOM_FOREST:
			this.classifierModel = new RandomForestClassifierModel();
		case DECISION_TREE:
		default:
			this.classifierModel = new DecisionTreeClassifierModel();
			break;
		}

		switch (regressorType) {
		case LINEAR_MODEL:
			this.regressorModel = new LinearRegressorModel();
			break;
		case RANDOM_FOREST:
			this.regressorModel = new RandomForestRegressorModel();
			break;
		case DECISION_TREE:
		default:
			this.regressorModel = new DecisionTreeRegressorModel();
			break;
		}
	}

	public Map<Integer, PredictionTuple<Integer, Double>> predict() {
		return predict(null, -1);
	}

	public Map<Integer, PredictionTuple<Integer, Double>> predict(int lastLevelId) {
		return predict(null, lastLevelId);
	}

	public Map<Integer, PredictionTuple<Integer, Double>> predict(Map<Integer, Map<String, Integer>> levelFeatures) {
		return predict(levelFeatures, -1);
	}

	/**
	 * Get the new Predictions for the current Level Features
	 * @param levelFeatures (Optional) The LevelFeatures for all Levels. Structure: {levelID : {featureName : value}}
	 * @param lastLevelId (Optional) The last Level that was played. 
	 * 					Will be used for not needing to calculate all predictions but only the one for which the features have changed. 
	 * 					Is not implemented yet!
	 * @return
	 */
	private Map<Integer, PredictionTuple<Integer, Double>> predict(Map<Integer, Map<String, Integer>> levelFeatures,
			int lastLevelId) {
		if (lastLevelId >= Settings.START_LEVEL && lastLevelId <= Settings.START_LEVEL + levelRange - 1) {
			this.levelFeatures.put(lastLevelId,
					new HashMap<String, Integer>(LevelStorage.getInstance().getLevelById(lastLevelId).featureMap));
			//TODO Implement only recalculating the possibly changed predictions 
		} else if (levelFeatures != null) {
			this.levelFeatures.putAll(levelFeatures);
		} else {
			this.updateLevelFeatures();
		}
		Map<Integer, Double> winPredictions = this.classifierModel.predict(this.levelFeatures);
		Map<Integer, Integer> scorePredictions = this.regressorModel.predict(this.levelFeatures);

		// unify both maps by levelId and put them into a Map<Integer,
		// PredictionTuple<Integer, Double>>
		for (Map.Entry<Integer, Integer> scoreEntry : scorePredictions.entrySet()) {
			for (Map.Entry<Integer, Double> winEntry : winPredictions.entrySet()) {
				if (scoreEntry.getKey().equals(winEntry.getKey())) {
					this.predictions.put(scoreEntry.getKey(),
							new PredictionTuple<Integer, Double>(scoreEntry.getValue(), winEntry.getValue()));
				}
			}
		}
		return predictions;
	}

	/**
	 * Update the feature maps of all considered levels we want to get predictions
	 * for.
	 */
	private void updateLevelFeatures() {
		if (!this.levelFeatures.isEmpty()) {
			this.levelFeatures.clear();
		}

		for (int levelId = Settings.START_LEVEL; levelId < Settings.START_LEVEL + this.levelRange; levelId++) {
			Level level = LevelStorage.getInstance().getLevelById(levelId);

			if (level != null) {
				this.levelFeatures.put(levelId, new HashMap<String, Integer>(level.featureMap));
			}
		}
	}

	public Map<Integer, PredictionTuple<Integer, Double>> getPredictions() {
		return predictions;
	}
}
