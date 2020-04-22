package de.uniba.sme.bambirds.planner;

import de.uniba.sme.bambirds.common.objects.Target;
import de.uniba.sme.bambirds.common.objects.ThinkerType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class OldPrologPlanParser extends PrologPlanParser {
	private static final Logger log = LogManager.getLogger(OldPrologPlanParser.class);


	/**
	 * Converts a Prolog list to {@link Target}s. {@code plans} must have the
	 * correct format:
	 * <p>
	 * {@code [[[target,...],[goal,...],[strategy],[rank]],...]}
	 * <p>
	 * {@code plans} represents plans as lists in Prolog syntax. Each plan contains
	 * four lists representing targets, goals, strategy and rank. The target and
	 * goal lists may contain multiple elements. If the format is wrong an empty or
	 * incomplete list may be returned.
	 *
	 * @param plans
	 *            A Prolog list representing plans
	 * @return A list of {@code Target}s
	 */
	public List<Target> parsePlans(String plans) {
		List<Target> targetList = new ArrayList<>();

		if (plans.isEmpty()) {
			log.error("[PPPo] Got empty string, returning empty list of Targets!");
			return targetList;
		}

		for (String plan : plans.split(Pattern.quote("]],[["))) {
			Target target;
			try {
				target = parseSinglePlan(plan);
			} catch (IllegalArgumentException e) {
				log.error("[PPPo] " + e.getMessage());
				continue;
			}
			if (target == null) {
				continue;
			}
			targetList.add(target);
		}

		return targetList;
	}

	private Target parseSinglePlan(String plan) throws IllegalArgumentException {
		// split plan into targets, goals, strategy and rank
		String[] targetArray = plan.split(Pattern.quote("],["));

		if (targetArray.length != 4) {
			throw new IllegalArgumentException("Incorrect plan format; ignoring plan");
		}

		for (int i = 0; i < targetArray.length; i++) {
			targetArray[i] = targetArray[i].replaceAll(Pattern.quote("[[["), "").replaceAll(Pattern.quote("]"), "");
		}

		String[] targetStrings = targetArray[0].split(",");
		validateTargetStrings(targetStrings);
		List<String> targets = Arrays.asList(targetStrings);
		String strategy = targetArray[2];
		double rankValue = Double.parseDouble(targetArray[3]);

		if (strategy.equals("dummy")) {
			// ignore dummy plan
			return null;
		}

		return new Target(targets.get(0), 42,strategy + " (legacy)", rankValue, ThinkerType.OLDPLANNER);
	}
}
