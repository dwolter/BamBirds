package planner;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

public class NewPrologPlanParserTest {
	
	PrologPlanParser newPlanner;
	
	@Before
	public void initialize() {
		newPlanner = new NewPrologPlanParser();
	}
	
	@After
	public void shutdown() {
		newPlanner = null;
	}
	
	@Test
	public void newParsePlansSucceedsWithCorrectInput() {
		// manually adapted from parsePlansSucceedsWithCorrectInput to fit new format
		String plans = "[[wood2,maxPen,8970.0],[wood3,roof,8044],[wood7,roof,6044.0],[wood2,minPen,5824.285714285715],[wood2,minPen,5819.285714285715],[wood3,pigCont,2640.0],[pig0,pigCont,2000],[wood1,destroyPrimitive,1901],[dummy,dummy,0]]";
		List<Target> targets = newPlanner.parsePlans(plans);
		double previous = Double.POSITIVE_INFINITY;
		for (Target target : targets) {
			double current = target.getConfidence();
			Assert.assertTrue(current < previous);
			previous = current;
		}
	}

	@Test
	public void newParsePlansFailsWithEmptyPlan() {
		String emptyPlan = "";
		Assert.assertTrue("This should return true, since an empty list of targets is returned!",
				newPlanner.parsePlans(emptyPlan).isEmpty());
	}

	@Test
	public void parseSinglePlanThrowsIllegalArgumentException() {
		String illegalPlan = "[[justTwo,argumentsInsteadOfThree],[one,correct,0.1]]";
		Assert.assertEquals("There is only one correct plan", 1, newPlanner.parsePlans(illegalPlan).size());
	}

	@Test
	public void noConfidenceForAPlanThrowsNumberFormatException() {
		String illegalPlanWithoutConfidence = "[[no,confidence,given],[also,none,here]]";
		Assert.assertEquals("No plan is correct, since there is no confidence given!", 0,
				newPlanner.parsePlans(illegalPlanWithoutConfidence).size());
	}

	@Test
	public void oneEntryPlanGetsHandledProperly() {
		String onlyOneEntry = "[[only,entry,0.9]]";
		Assert.assertEquals("The output list should have size 1", 1,
				newPlanner.parsePlans(onlyOneEntry).size());
	}

	@Test
	public void planWithoutTargetThrowsIllegalArgumentException() {
		String illegalPlans = "[[proper,plan,1.0],[,broken,0.9]]";
		Assert.assertEquals("There is one correct plan!", 1, newPlanner.parsePlans(illegalPlans).size());
	}

	@Test
	public void planWithoutTargetAtFirstPosThrowsIllegalArgumentException() {
		String illegalPlan = "[[,broken,0.3]]";
		Assert.assertEquals("There is no correct plan!", 0, newPlanner.parsePlans(illegalPlan).size());
	}
}