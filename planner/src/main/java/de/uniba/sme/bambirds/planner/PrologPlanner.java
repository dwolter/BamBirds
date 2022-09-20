package de.uniba.sme.bambirds.planner;

import de.uniba.sme.bambirds.common.PlanParser;
import de.uniba.sme.bambirds.common.Strategy;
import de.uniba.sme.bambirds.common.StrategyConsumer;
import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.utils.SWIConnector;
import de.uniba.sme.bambirds.common.utils.Settings;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import de.uniba.sme.bambirds.planner.physicssimulation.SimulationUtils;
import de.uniba.sme.bambirds.planner.predicates.PredicateGeneratorManager;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.SerializableScene;

import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PrologPlanner implements Strategy {
	private static final Logger log = LogManager.getLogger();

	private final SWIConnector connector;
	private Thread connectorThread;
	private final String knowledgeBaseFilename;
	private final PlanParser parser;
	private boolean finished = false;
	private boolean cancelled = false;

	/**
	 * Create a new PrologPlanner including a knowledge base and connector to Prolog
	 * @param level for which the model should be built
	 * @param basename base name for the knowledge file
	 * @param parser a Parser for Prolog targets
	 */
	public PrologPlanner(Level level, String basename, PrologPlanParser parser) {
		this.connector = new SWIConnector(Settings.PATH_TO_SWIPL, Settings.PLANNER_START, Settings.PLANNER_EXECUTABLE);
		startSWIConnector(connector);

		this.knowledgeBaseFilename = PredicateGeneratorManager.defaultConfiguration(level, basename).buildModel().toString();

		if (Settings.SIM_DEBUG_MODE) {
			//Save serializable scene that can be loaded in visual debugger
			try {
				Path scenePath = Paths.get("sim_scenes/" + basename).toAbsolutePath().normalize();
				if (!scenePath.toFile().exists()) {
					SimulationUtils.saveSerializableScene(new SerializableScene(level.currentScene), scenePath.toString());
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		this.parser = parser;
	}

	/**
	 * Create a new PrologPlanner including a connector to Prolog given an existing knowledge base
	 * @param knowledgeBaseFilename Path to the knowledge base
	 * @param parser  a Parser for Prolog targets
	 */
	public PrologPlanner(String knowledgeBaseFilename, PrologPlanParser parser) {
		this.connector = new SWIConnector(Settings.PATH_TO_SWIPL, Settings.PLANNER_START, Settings.PLANNER_EXECUTABLE);
		startSWIConnector(connector);
		this.knowledgeBaseFilename = knowledgeBaseFilename;
		this.parser = parser;
	}

	/**
	 * Create a new PrologPlanner using the given connector to Prolog and existing knowledge base
	 * @param connector Connector to Prolog
	 * @param knowledgeBaseFilename Path to the knowledge base
	 * @param parser  a Parser for Prolog targets
	 */
	public PrologPlanner(SWIConnector connector, String knowledgeBaseFilename, PlanParser parser) {
		this.connector = connector;
		this.knowledgeBaseFilename = knowledgeBaseFilename;
		this.parser = parser;
	}

	private void startSWIConnector(SWIConnector connector) {
		connectorThread = new Thread(connector);
		connectorThread.start();
	}

	@Override
	public void plan(StrategyConsumer consumer, long timeOut) {
		connector.sendCommand("\"" + knowledgeBaseFilename + "\".");
		long end = System.currentTimeMillis() + timeOut;
		Set<String> intermediatePlans = new HashSet<>();
		try {
			while(true) {
				long cur = System.currentTimeMillis();
				String result = connector.getResult(end - cur).trim();
				if (result.startsWith("%")) {
					result = result.substring(1);
					if (intermediatePlans.add(result)) {
						Plan intermediatePlan = parser.parsePlan(result);
						Node node = consumer.post(intermediatePlan);
						if (node != null) {
							// Set all intermediate plans filtered (wil onlly be executed if no other plans are available anymore)
							node.setFiltered(true);
						}
					}
				} else {
					List<Plan> finalPlans = parser.parsePlans(result);
					List<Node> finalNodes = consumer.post(finalPlans);
					// Set all final Nodes to not be filtered
					for (Node node : finalNodes) {
						node.setFiltered(false);
					}
					break;
				}
			}
		} catch (IOException e) {
			log.error("No targets left", e);
		} catch (InterruptedException e) {
			log.warn("Planning got interrupted, returning the current list of targets");
			cancelled = true;
		}
		finished = true;
		kill();
	}

	@Override
	public List<Plan> planSynchronously(long timeOut) {
		connector.sendCommand("\"" + knowledgeBaseFilename + "\".");
		List<Plan> finalPlans = new ArrayList<>();
		long end = System.currentTimeMillis() + timeOut;
		Set<String> unfilteredPlans = new HashSet<>();
		try {
			while(true) {
				long cur = System.currentTimeMillis();
				String result = connector.getResult(end - cur).trim();
				if (result.startsWith("%")) {
					result = result.substring(1);
					unfilteredPlans.add(result);
				} else {
					finalPlans = parser.parsePlans(result);
					break;
				}
			}
		} catch (IOException e) {
			log.error("No targets found", e);
		} catch (InterruptedException e) {
			log.warn("Planning got interrupted, stopping execution");
			cancelled = true;
		}
		if (finalPlans.isEmpty() && ! unfilteredPlans.isEmpty()) {
			for (String plan : unfilteredPlans) {
				Plan unfilteredPlan = parser.parsePlan(plan);
				unfilteredPlan.setConfidence(-1);
				finalPlans.add(unfilteredPlan);
			}
		}
		finished = true;
		kill();
		return finalPlans;
	}

	@Override
	public void kill() {
		if (connector != null) {
			connector.shutdown();
		}
	}

	@Override public boolean isCancelled() {
		return cancelled;
	}

	public static void compileExecutable() {
		String[] classpath = System.getProperty("java.class.path").split(SystemUtils.IS_OS_WINDOWS ? ";" : ":");
		boolean prologFound = false;
		boolean btcLibFound = false;
		Path destinationPath = null;
		Path btcLibDir = null;
		for(String path: classpath){
			if (path.endsWith("start.pl") ) {
				log.debug("Planner start found: {}", path);
				Settings.PLANNER_START = Paths.get(path);
				destinationPath = new File(path).getParentFile().toPath();
				prologFound = true;
			} else if (path.matches(".*prolog/?$")) {
				log.debug("Lib Folder found: {}", path);
				File libDirectory = new File(path);
				log.debug(libDirectory.getAbsolutePath());
				if (!libDirectory.isDirectory()) {
					// Gradle is does not set the classpath correctly in the distribution
					File parentDirectory = libDirectory.getParentFile();
					if(!parentDirectory.isDirectory())
						continue;
					libDirectory = parentDirectory;
				}
				destinationPath = libDirectory.toPath();
				File startFile = new File(libDirectory, "planner/start.pl");
				if (startFile.isFile()) {
					Settings.PLANNER_START = startFile.toPath();
					prologFound = true;
				}
			} else if (path.endsWith("behind_the_corner.so")) {
				File libDirectory = new File(path).getParentFile();
				btcLibDir = libDirectory.toPath();
				btcLibFound = true;
			}
			if (prologFound && btcLibFound) break;
		}
		if (!prologFound) {
			log.fatal("Could not find prolog planner start");
			System.exit(1);
		}
		if (!btcLibFound && !SystemUtils.IS_OS_WINDOWS) {
			log.error("Could not locate behind_the_corner library");
			System.exit(1);
		}
		Settings.PLANNER_EXECUTABLE = destinationPath.resolve("bambirds-planner");
		Settings.PLANNER_LIB_DIR = btcLibDir;
		try {
			log.info("Compiling prolog planner ...");
			Process process = Runtime.getRuntime().exec(new String[] {
				"swipl", 
				"-p", 
				"lib="+ (Settings.PLANNER_LIB_DIR != null ? Settings.PLANNER_LIB_DIR.toString() : "") ,
				"-o", 
				Settings.PLANNER_EXECUTABLE.toString(), 
				"-c", 
				Settings.PLANNER_START.toString() 
			});
			process.waitFor();
			if (process.exitValue() != 0) {
				throw new IOException("Prolog compilation returned exit value " + process.exitValue());
			}
			log.debug("done");
		} catch (IOException | InterruptedException e) {
			log.error("Failed to compile planner", e);
			Settings.PLANNER_EXECUTABLE = null;
		}

	}

	public boolean isFinished() {
		return finished;
	}
}
