package de.uniba.sme.bambirds.level_selection.model;

import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dmg.pmml.FieldName;
import org.jpmml.evaluator.Evaluator;
import org.jpmml.evaluator.EvaluatorUtil;
import org.jpmml.evaluator.FieldValue;
import org.jpmml.evaluator.InputField;
import org.jpmml.evaluator.LoadingModelEvaluatorBuilder;
import org.jpmml.model.VisitorBattery;
import org.xml.sax.SAXException;

/**
 * This Class runs the {@code model_represantation.DecisionTreeClassifier} with
 * the input features
 */
public class DecisionTreeClassifierModel implements ClassifierModel {
	private static final Logger log = LogManager.getLogger(DecisionTreeClassifierModel.class);

	private Map<Integer, Map<String, Integer>> levelFeatures;
	private Evaluator evaluator;

	// reuse executor & levelFeatures from manager
	public DecisionTreeClassifierModel() {
		this.levelFeatures = new HashMap<Integer, Map<String, Integer>>();

		InputStream pmml_file_stream = getClass().getClassLoader().getResourceAsStream("models/tree_classifier.pmml");

		try {
			this.evaluator = new LoadingModelEvaluatorBuilder()
				.setLocatable(false)
				.setVisitors(new VisitorBattery())
				.load(pmml_file_stream)
				.build();
		} catch (SAXException | JAXBException e) {
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
		log.info("Input fields: " + inputFields);

		for (Map.Entry<Integer, Map<String, Integer>> entry : this.levelFeatures.entrySet()) {
			int levelId = entry.getKey();
			Map<String, Integer> featureMap = entry.getValue();

			Map<FieldName, FieldValue> arguments = ModelHelper.mapArguments(inputFields,featureMap);

			// Evaluating the model with known-good arguments
			Map<FieldName, ?> results = evaluator.evaluate(arguments);

			// Decoupling results from the JPMML-Evaluator runtime environment
			Map<String, ?> resultRecord = EvaluatorUtil.decodeAll(results);

			double probWin = (Double) resultRecord.get("probability(won)");

			predictions.put(levelId, probWin);
		}

		return predictions;
	}
}
