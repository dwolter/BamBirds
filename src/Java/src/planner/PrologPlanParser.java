package planner;

import java.util.List;
import java.util.regex.Pattern;

public abstract class PrologPlanParser {

	public abstract List<Target> parsePlans(String plans);


	/**
	 * Validates if the target string array has sufficient arguments, and that those
	 * arguments are both not empty and don't contain illegal characters left over
	 * from parsing the prolog string.
	 *
	 * @param targetStrings
	 * @throws IllegalArgumentException
	 */
	void validateTargetStrings(String[] targetStrings) throws IllegalArgumentException {
		for (String targetEntry : targetStrings) {
			// validate that no entry of the string array is empty, otherwise sort will
			// fail!
			if (targetEntry.isEmpty()) {
				throw new IllegalArgumentException("Incorrect plan; argument is missing; ignoring plan");
			}
		}
	}

	void validateArrayLength(String[] targetStrings) {
		// validate that there are 3 entries in the string array
		if (targetStrings.length != 3) {
			throw new IllegalArgumentException("Incorrect plan format; an entry is missing; ignoring plan");
		}
	}

	void removeIllegalChars(String[] decisionStrings) {
		if (decisionStrings[0].contains("[[")) {
			decisionStrings[0] = decisionStrings[0].replaceAll(Pattern.quote("[["), "");
		}
		if (decisionStrings[2].contains("]]")) {
			decisionStrings[2] = decisionStrings[2].replaceAll(Pattern.quote("]]"), "");
		}
	}
}
