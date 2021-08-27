package de.uniba.sme.bambirds.planner;

import com.google.gson.*;

import de.uniba.sme.bambirds.common.PlanParser;
import de.uniba.sme.bambirds.common.gson.JsonDeserializerWithRequired;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.ThinkerType;

import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PrologPlanParser implements PlanParser {
	private static final Logger log = LogManager.getLogger(PrologPlanParser.class);

	private final Gson gson;

	public PrologPlanParser() {
		gson = new GsonBuilder()
				.registerTypeAdapter(Plan.class, new JsonDeserializerWithRequired<Plan>())
				.create();
	}

	/**
	 * <p>
	 * Parses a Prolog plan of the new (2017) format. The Prolog-module of our agent
	 * only returns a String representation of his results, which has to be
	 * processed into {@link Plan}s.
	 * </p>
	 *
	 * <p>
	 * The format used by Prolog is:
	 * <code>[Plan_0,Plan_1...,Plan_N]</code>
	 * </p>
	 *
	 * @param plans
	 * @return List of {@link Plan}s generated from the given String
	 */
	@Override
	public List<Plan> parsePlans(String plans) {
		List<Plan> targets = new ArrayList<>();
		try {
			JsonArray json = JsonParser.parseString(plans).getAsJsonArray();
			for (JsonElement plan : json) {
				try {
					targets.add(parseJsonPlan(plan));
				} catch (JsonParseException | IllegalArgumentException e) {
					log.error(e);
				}
			}
		} catch (JsonParseException | IllegalStateException e) {
			log.error(e);
		}
		return targets;
	}

	/**
	 * <p>
	 * Parses a String of a given format into a {@code Plan} Object. It operates
	 * on the format of the new Prolog code of 2021, thus it can not be used with
	 * the planner used before.
	 * </p>
	 * 
	 * <p>
	 * The format used is [target,impactAngle,chosenStrat,confidence,[reasonA,reasonB,...]?,shotInfo?] for one Plan, where the values with ? are optional. 
	 * The first and last brace may be omitted 
	 * </p>
	 *
	 * @param plan
	 * @return
	 * @throws IllegalArgumentException
	 */
	@Override
	public Plan parsePlan(String plan) throws JsonParseException  {
		return parseJsonPlan(JsonParser.parseString(plan));
	}

	private Plan parseJsonPlan(JsonElement plan) throws JsonSyntaxException {
		if(plan.isJsonObject()) {
			JsonObject planObject = (JsonObject) plan;
			if (!planObject.has("thinker")) {
				planObject.add("thinker", new JsonPrimitive(ThinkerType.NEWPLANNER.toString()));
			}

			return gson.fromJson(planObject, Plan.class);
		} else {
			throw new JsonSyntaxException("plan should be a JSON object");
		}
	}

}
