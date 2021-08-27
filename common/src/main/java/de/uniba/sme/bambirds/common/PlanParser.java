package de.uniba.sme.bambirds.common;

import java.util.List;

import de.uniba.sme.bambirds.common.objects.Plan;

public interface PlanParser {

	/** Parse multiple plans returned from an external Strategy <br>
	 * 
	 * The input is a list of plans where each Plan is parsed by {@link #parsePlan(String)}
	 * @param plans A String of Prolog plans
	 * @return The parsed plans
	 */
	public List<Plan> parsePlans(String plans);

	
	/** Parse a single plan returned from an external Strategy.
	 * 
	 * @param plan
	 * @return The parsed plan
	 */
	public Plan parsePlan(String plan);
}
