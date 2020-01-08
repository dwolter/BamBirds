package planner;

import helper.CustomLogger;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class NewPrologPlanParser extends PrologPlanParser {
	/**
	 * <p>
	 * Parses a Prolog plan of the new (2017) format. The Prolog-module of our agent
	 * only returns a String representation of his results, which has to be
	 * processed into {@link Target}s.
	 * </p>
	 *
	 * <p>
	 * The format used by Prolog is:
	 * <code>[[target,chosenStrat,confidence],...,[targetN,chosenStratN,confidenceN]]</code>
	 * </p>
	 *
	 * @param plans
	 * @return List of {@link Target}s generated from the given String
	 */
	@Override
	public List<Target> parsePlans(String plans) {
		List<Target> targets = new ArrayList<Target>();

		if (plans.isEmpty()) {
			CustomLogger.severe("[PPP] Got empty string, returning empty list of Targets!");
			return targets;
		}

		for (String plan : plans.split(Pattern.quote("],["))) {
            if (plan.contains("[[")) {
                plan = plan.replaceAll(Pattern.quote("[["), "");
            }
            if (plan.contains("]]")) {
                plan = plan.replaceAll(Pattern.quote("]]"), "");
            }
            

			Target newTarget;
			try {
				newTarget = parseSinglePlan(plan);
			} catch (IllegalArgumentException e) {
				CustomLogger.severe("[PPP] " + e.getMessage());
				continue;
			}
			if (newTarget == null) {
				continue;
			}

			targets.add(newTarget);
		}

		return targets;
	}

	/**
	 * Parses a String of a given format into a {@code Target} Object. It operates
	 * on the format of the new Prolog code of 2017, thus it can not be used with
	 * the planner used before.
	 *
	 * @param plan
	 * @return
	 * @throws IllegalArgumentException
	 */
	private Target parseSinglePlan(String plan) throws IllegalArgumentException {
		// split plan into targets, strategy and confidence

		// The new format of the Plans that enter here are as follows:
		// target,strategyName,confidence

		// Special case: first and last element of the List:
		// [[target,sector,strat,conf
		// targetLast,sectorLast,stratLast,confLast]]
		String[] targetStrings = plan.split(Pattern.quote(","));

        if (targetStrings.length != 4) {
            throw new IllegalArgumentException("Incorrect plan format, ignoring plan");
        }

		// Handle special case mentioned above
		removeIllegalChars(targetStrings);

		validateTargetStrings(targetStrings);

		String target = targetStrings[0];

        int sector = -1;
        double confidence = 0.0;
        try {
            sector = Integer.parseInt(targetStrings[1]);
            confidence = Double.parseDouble(targetStrings[3]);
        } catch (NumberFormatException e) {
            CustomLogger.severe("Error parsing plan, using default values. Exception:" + e.getMessage());
        };

        String strategy = targetStrings[2];
		 

		if (target.equals("dummy")) {
			// ignore dummy target
			return null;
		}

		return new Target(target, sector, strategy, confidence, ThinkerType.NEWPLANNER);
	}

}
