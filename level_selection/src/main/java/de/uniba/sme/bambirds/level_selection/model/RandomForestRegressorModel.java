package de.uniba.sme.bambirds.level_selection.model;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import java.util.HashMap;

import jakarta.xml.bind.JAXBException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jpmml.evaluator.Evaluator;
import org.jpmml.evaluator.EvaluatorUtil;
import org.jpmml.evaluator.FieldValue;
import org.jpmml.evaluator.InputField;
import org.jpmml.evaluator.LoadingModelEvaluatorBuilder;
import org.jpmml.model.visitors.VisitorBattery;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;


/**
 * This Class runs the {@code model_representation.RandomForestRegressor} with the input features
 */
public class RandomForestRegressorModel implements RegressorModel {
	private static final Logger log = LogManager.getLogger(RandomForestRegressorModel.class);
	private Map<Integer, Map<String, Integer>> levelFeatures;

	private Evaluator evaluator;

	public RandomForestRegressorModel() {
		this.levelFeatures = new HashMap<Integer, Map<String, Integer>>();
		InputStream pmml_file_stream = getClass().getClassLoader().getResourceAsStream("models/tree_classifier.pmml");

		try {
			this.evaluator = new LoadingModelEvaluatorBuilder()
				.setLocatable(false)
				.setVisitors(new VisitorBattery())
				.load(pmml_file_stream)
				.build();
		} catch (SAXException | ParserConfigurationException | JAXBException e) {
			// TODO Auto-generated catch block
			log.fatal("Model loading failed", e);
		}
		evaluator.verify();
	}

	@Override
	public Map<Integer, Integer> predict(Map<Integer, Map<String, Integer>> levelFeatures) {
		// calculate results from regressor model
		this.levelFeatures.putAll(levelFeatures);
		Map<Integer, Integer> predictions = new HashMap<Integer, Integer>();
		List<? extends InputField> inputFields = evaluator.getInputFields();
		log.trace("Input fields: " + inputFields);

		for (Map.Entry<Integer, Map<String, Integer>> entry : this.levelFeatures.entrySet()) {
			int levelId = entry.getKey();
			Map<String, Integer> featureMap = entry.getValue();

			Map<String, FieldValue> arguments = ModelHelper.mapArguments(inputFields,featureMap);

			// Evaluating the model with known-good arguments
			Map<String, ?> results = evaluator.evaluate(arguments);

			// Decoupling results from the JPMML-Evaluator runtime environment
			Map<String, ?> resultRecord = EvaluatorUtil.decodeAll(results);

			int score = ((Double) resultRecord.get("num_score")).intValue();

			predictions.put(levelId, score);
		}

		return predictions;
	}
}
