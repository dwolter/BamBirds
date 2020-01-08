package planner;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

public class OldPrologPlanParserTest {

	PrologPlanParser oldPlanner;

	@Before
	public void initialize() {
		oldPlanner = new OldPrologPlanParser();
	}

	@After
	public void shutdown(){
		oldPlanner = null;
	}

	@Test
	public void parsePlansReturnsEmptyListOnIncorrectFormat() {
		// outdated, incorrect format; none of the plans can be parsed
		String incorrect = "[[[midStoneBoulder3],[midStoneBoulder3,midStoneBoulder3,midWoodBoulder3,pig_3,midStoneBoulder2,midStoneBoulder2,midWoodBoulder2,pig_2]],[[pig_3],[pig_3]],[[pig_3],[pig_3]]]";
		List<Target> targets = oldPlanner.parsePlans(incorrect);
		Assert.assertTrue(targets.isEmpty()); // all plans have been ignored
	}

	@Test
	public void parsePlansSucceedsWithCorrectInput() {
		// taken from the first shot of the first Poached Eggs level using the 2016
		// agent
		// eight plans (the dummy is ignored)
		String correct = "[[[wood2],[wood13,pig0],[maxPen],[8970.0]],[[wood3],[pig0],[roof],[8044]],[[wood7],[pig0],[roof],[6044.0]],[[wood2],[wood13,pig0],[minPen],[5824.285714285715]],[[wood2],[wood1,pig0],[minPen],[5819.285714285715]],[[wood3],[pig0],[pigCont],[2640.0]],[[pig0],[pig0],[pigCont],[2000]],[[wood1],[wood2,pig0],[destroyPrimitive],[1901]],[[dummy],[dummy],[dummy],[0]]]";
		List<Target> targets = oldPlanner.parsePlans(correct);
		Assert.assertEquals(8, targets.size());
	}
}
