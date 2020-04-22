package de.uniba.sme.bambirds.level_selection.model;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.dmg.pmml.FieldName;
import org.jpmml.evaluator.FieldValue;
import org.jpmml.evaluator.InputField;

/**
 * ModelHelper
 */
public class ModelHelper {

	public static Map<FieldName, FieldValue> mapArguments(List<? extends InputField> inputFields, Map<String, Integer> featureMap){
		Map<FieldName, FieldValue> arguments = new LinkedHashMap<>();

			// Mapping the record field-by-field from data source schema to PMML schema
			for(InputField inputField : inputFields){
				FieldName inputName = inputField.getName();

				Object rawValue = featureMap.get(inputName.getValue());

				// Transforming an arbitrary user-supplied value to a known-good PMML value
				FieldValue inputValue = inputField.prepare(rawValue);

				arguments.put(inputName, inputValue);
			}
		return arguments;
	}
}