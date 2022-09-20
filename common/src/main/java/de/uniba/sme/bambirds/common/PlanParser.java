package de.uniba.sme.bambirds.common;

import de.uniba.sme.bambirds.common.objects.Plan;

import java.util.List;

public interface PlanParser {

	/**
	 * Parse multiple plans returned from an external Strategy. <br>
	 * <p>
	 * The input is a list of plans where each Plan is parsed by {@link #parsePlan(String)}
	 *
	 * @param plans A String of Prolog plans
	 * @return The parsed plans
	 */
	List<Plan> parsePlans(String plans);


	/**
	 * Parse a single plan returned from an external Strategy.
	 *
	 * @param plan Plan as string to parse
	 * @return The parsed plan
	 */
	Plan parsePlan(String plan);
}
