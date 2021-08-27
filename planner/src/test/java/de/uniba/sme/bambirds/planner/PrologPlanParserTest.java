package de.uniba.sme.bambirds.planner;

import de.uniba.sme.bambirds.common.PlanParser;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ThinkerType;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.List;

import com.google.gson.JsonSyntaxException;

public class PrologPlanParserTest {

	private final String FULL_PLAN = "{\"bird\":\"redbird0\",\"confidence\":0.75,\"impact_angle\":45,\"reasons\": [\"pig1\", \"pig2\" ],\"shot\": {\"drag_x\":23,\"drag_y\":34,\"sling_x\":234,\"sling_y\":432,\"tap_time\":50},\"strategy\":\"exampleStrategy\",\"target_object\":\"struct1\"}";
	
	PlanParser newPlanner;
	
	@BeforeEach
	public void initialize() {
		newPlanner = new PrologPlanParser();
	}
	
	@AfterEach
	public void shutdown() {
		newPlanner = null;
	}
	
	@Test
	public void newParsePlansSucceedsWithCorrectInput() throws URISyntaxException, IOException {
		// manually adapted from parsePlansSucceedsWithCorrectInput to fit new format
		String plans = String.join("\n",Files.readAllLines(new File(this.getClass().getResource("/plans/multiple_valid.json").toURI()).toPath()));
		List<Plan> targets = newPlanner.parsePlans(plans);
		System.out.println(targets);
		assertEquals(6, targets.size(), "There should be 6 valid Plans");
		double previous = Double.POSITIVE_INFINITY;
		for (Plan plan : targets) {
			double current = plan.getConfidence();
			assertTrue(current <= previous, "Confidence should be less or equal to the previous plan");
			previous = current;
		}
	}

	@Test
	public void parsePlansParsesToPlan() {
		Shot s = new Shot(234,432,23,34,0,0,0,50);
		Plan p = new Plan("redbird0","struct1",45,"exampleStrategy",0.75,new String[]{"pig1","pig2"},s,ThinkerType.NEWPLANNER);
		Plan parsedPlan = newPlanner.parsePlan(FULL_PLAN);
		assertEquals(parsedPlan, p, "Parsed plan should equal the created plan");
	}

	@Test
	public void parsePlansFailsWithEmptyPlan() {
		String emptyPlan = "";
		assertTrue(newPlanner.parsePlans(emptyPlan).isEmpty(), "An empty plans string should return an empty list of plans");
	}

	@Test
	public void parsePlansIgnoresWrongPlans() {
		String illegalPlan = "[{\"confidence\":\"Fails\",\"impact_angle\":45,\"shot\": {\"drag_x\":23,\"drag_y\":34,\"sling_x\":234,\"sling_y\":432,\"tap_time\":0.5},\"thinker\":\"NEWPLANNER\"},{}]";
		assertEquals(0, newPlanner.parsePlans(illegalPlan).size(), "Incorrect plans should be ignored");
	}

	@Test
	public void nonNumericalConfidenceFails() {
		String planWithStringConfidence = "{\"confidence\":\"Fails\",\"impact_angle\":45,\"shot\": {\"drag_x\":23,\"drag_y\":34,\"sling_x\":234,\"sling_y\":432,\"tap_time\":0.5},\"thinker\":\"NEWPLANNER\"}";
		assertThrows(JsonSyntaxException.class, () -> newPlanner.parsePlan(planWithStringConfidence), "Parsing plan directly should throw Exception");
		assertEquals(newPlanner.parsePlans("[" + planWithStringConfidence + "]").size(), 0);
	}

	@Test
	public void planWithAdditionalReasonHandled() {
		Plan parsedPlan = newPlanner.parsePlan(FULL_PLAN);
		System.out.println(parsedPlan);
		assertNotNull(parsedPlan, "There is one correct plan!");
		assertArrayEquals(new String[] {"pig1", "pig2"}, parsedPlan.getReasons(), "Reasons are not equal");
	}
	
}