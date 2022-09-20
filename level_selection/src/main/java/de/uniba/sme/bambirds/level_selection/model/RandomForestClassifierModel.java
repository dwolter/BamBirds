package de.uniba.sme.bambirds.level_selection.model;

import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
 * This Class runs the {@code model_representation.RandomForestClassifier} with the input features
 */
public class RandomForestClassifierModel implements ClassifierModel {
	private static final Logger log = LogManager.getLogger(RandomForestClassifierModel.class);

	private Map<Integer, Map<String, Integer>> levelFeatures;
	private Evaluator evaluator;

	public RandomForestClassifierModel() {
		this.levelFeatures = new HashMap<Integer, Map<String, Integer>>();

		InputStream pmml_file_stream = getClass().getClassLoader().getResourceAsStream("models/forest_classifier.pmml");

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
	public Map<Integer, Double> predict(Map<Integer, Map<String, Integer>> levelFeatures) {
		// calculate results from classifier model
		this.levelFeatures.putAll(levelFeatures);
		Map<Integer, Double> predictions = new HashMap<Integer, Double>();
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

			double probWin = (Double) resultRecord.get("probability(won)");

			predictions.put(levelId, probWin);
		}

		return predictions;
	}
}
