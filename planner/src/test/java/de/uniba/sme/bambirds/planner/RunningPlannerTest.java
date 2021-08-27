package de.uniba.sme.bambirds.planner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D.Double;

import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;

import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import de.uniba.sme.bambirds.common.StrategyConsumer;
import de.uniba.sme.bambirds.common.utils.SWIConnector;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.ShotHelper;

public class RunningPlannerTest {
	private static final Logger log = LogManager.getLogger();

	SWIConnector connector;
	Thread connectorThread;

	@BeforeEach
	public void setup() {
		PrologPlanner.compileExecutable();
		connector = new SWIConnector(Settings.PATH_TO_SWIPL, Settings.PLANNER_START);
		connectorThread = new Thread(connector);
		connectorThread.start();
	}

	@ParameterizedTest
	@ValueSource(strings = {"example_situation1.pl" ,"example_situation2.pl"})
	public void runExampleSituation(String situationFile) throws InterruptedException {
		String filename = Paths.get("planner", "src", "test", "resources", situationFile).toString();
		if (SystemUtils.getUserDir().getName().equals("planner")){
			filename = Paths.get("..",filename).toString();
		}
		log.debug(filename);
		PrologPlanner planner = new PrologPlanner(connector, filename, new PrologPlanParser());
		Thread.sleep(3000);
		List<Plan> plans = planner.planSynchronously(30000);
		System.out.println(plans);
		assertFalse(plans.isEmpty(), "List of Plans should not be empty");
		assertFalse(plans.get(0).getConfidence() < 0, "First Plan should not have a negative confidence");
	}

	@ParameterizedTest
	@ValueSource(strings = {"example_situation1.pl" ,"example_situation2.pl"})
	public void runWithConsumer(String situationFile) {
		String filename = Paths.get("planner", "src", "test", "resources", situationFile).toString();
		if (SystemUtils.getUserDir().getName().equals("planner")){
			filename = Paths.get("..",filename).toString();
		}
		log.debug(filename);
		PrologPlanner planner = new PrologPlanner(connector, filename, new PrologPlanParser());

		List<Plan> intermediatePlans = new ArrayList<Plan>();
		List<Plan> finalPlans = new ArrayList<Plan>();

		StrategyConsumer consumer = new StrategyConsumer() {

			@Override
			public void post(Plan newTarget) {
				intermediatePlans.add(newTarget);
			}

			@Override
			public void post(List<Plan> newTargets) {
				finalPlans.addAll(newTargets);
			}
		};

		planner.plan(consumer, 20000);
		System.out.println(intermediatePlans);
		System.out.println(finalPlans);
		assertFalse(intermediatePlans.isEmpty(), "List of intermediate Plans should not be empty");
		assertFalse(finalPlans.isEmpty(), "List of final Plans should not be empty");
	}

	@ParameterizedTest
	@ValueSource(strings = {"example_situation1.pl" ,"example_situation2.pl"})
	public void runWithConsumerHandleTermination(String situationFile) throws ExecutionException, InterruptedException {
		String filename = Paths.get("planner", "src", "test", "resources", situationFile).toString();
		if (SystemUtils.getUserDir().getName().equals("planner")){
			filename = Paths.get("..",filename).toString();
		}
		log.debug(filename);
		PrologPlanner planner = new PrologPlanner(connector, filename, new PrologPlanParser());

		List<Plan> plans = new ArrayList<>();

		StrategyConsumer consumer = new StrategyConsumer() {

			@Override
			public void post(Plan newTarget) {
				plans.add(newTarget);
			}

			@Override
			public void post(List<Plan> newTargets) {
				plans.addAll(newTargets);
			}
		};

		ExecutorService service = Executors.newSingleThreadExecutor();
		service.submit(() -> {
			planner.plan(consumer, 10000);
		});
		Thread.sleep(100);
		service.shutdownNow();
		service.awaitTermination(10, TimeUnit.SECONDS);
		System.out.println(plans);
		assertTrue(planner.isCancelled(), "planning should be cancelled");
		assertFalse(connector.isRunning());

	}

	
	// @ParameterizedTest
	// @ValueSource(strings = {"example_situation1.pl" ,"example_situation2.pl"})
	// public void plansPhysicsWork(String situationFile) {
	// 	String filename = Paths.get("planner", "src", "test", "resources", situationFile).toString();
	// 	if (SystemUtils.getUserDir().getName().equals("planner")){
	// 		filename = Paths.get("..",filename).toString();
	// 	}
	// 	log.debug(filename);
	// 	PrologPlanner planner = new PrologPlanner(connector, filename, new NewPrologPlanParser());

	// 	List<Plan> intermediatePlans = new ArrayList<Plan>();
	// 	List<Plan> finalPlans = new ArrayList<Plan>();

	// 	StrategyConsumer consumer = new StrategyConsumer() {

	// 		@Override
	// 		public void post(Plan newTarget) {
	// 			intermediatePlans.add(newTarget);
	// 		}

	// 		@Override
	// 		public void post(List<Plan> newTargets) {
	// 			finalPlans.addAll(newTargets);
	// 		}
	// 	};

	// 	planner.plan(consumer, 10000);

	// 	double scaling = 1;
	// 	Slingshot sling;
	// 	if (situationFile.equals("example_situation1.pl") || situationFile.equals("example_situation2.pl")) {
	// 		scaling = 1.005;
	// 	}
	// 	if (situationFile.equals("example_situation1.pl")) {
	// 		sling = new Slingshot(new Rectangle(), new Double(188,315.5));
	// 	}
	// 	for (Plan p : finalPlans) {
	// 		ShotHelper.setProperties(scaling, ABType.getTypeFromId(p.getBird()));
	// 		Shot s = p.getShot();
	// 		double angle = ShotHelper.releasePointToAngle(new Point(s.getDrag_x(), s.getDrag_y()));
	// 		ShotHelper.predictImpactAngle(angle, sling, );
	// 		assertEquals(p.getAngle(), actual);
	// 	}
	// }

	@AfterEach
	public void shutdown() {
		connector.shutdown();
	}
}