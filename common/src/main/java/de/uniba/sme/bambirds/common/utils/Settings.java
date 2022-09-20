package de.uniba.sme.bambirds.common.utils;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Console;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import javax.print.DocFlavor.STRING;

import static de.uniba.sme.bambirds.common.objects.ab.ABType.BlackBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.BlueBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.RedBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.WhiteBird;
import static de.uniba.sme.bambirds.common.objects.ab.ABType.YellowBird;

/**
 * Stores settings for the BamBirds Agent.
 * <p>
 * Can load configurations from cli-arguments and config.properties
 *
 * @author Felix Haase
 */
public class Settings {
	private static final Logger log = LogManager.getLogger(Settings.class);

	// Connection Settings
	public static String SERVER_HOST = "localhost";
	public static int TEAM_ID = 1;

	/**
	 * Path to swi-prolog executable. By default, it is used from PATH
	 */
	public static String PATH_TO_SWIPL = "swipl";

	// Settings for Strategies
	public static final long STRATEGY_TIMEOUT = 20000;
	public static final TimeUnit STRATEGY_TIMEOUT_UNIT = TimeUnit.MILLISECONDS;
	public static final boolean STRATEGY_ASYNC = true;
	public static final long STRATEGY_ASNYC_TIMEOUT = 60000;
	
	// hyperparameter further filtering available nodes, removes nodes not at least 'X' percent of the highest evaluated confidence
	public static double SHOTSELECTION_PERCENTAGE_OF_MAX_SCORE = 0.5;
	// hyperparameter tuning exploration vs exploitation, exponentiates then renormalises the weights for a probability distribution
	public static double SHOTSELECTION_EXPONENT = 5;

	// Image specifications
	public static final int IMAGE_HEIGHT = 480;
	public static final int IMAGE_WIDTH = 840;

	/**
	 * 0 for AB and 1 for SB
	 */
	public static ServerType SERVER_TYPE = ServerType.ANGRY_BIRDS;

	public enum ServerType {
		ANGRY_BIRDS, SCIENCE_BIRDS
	}

	public static GameMode GAME_MODE = GameMode.COMPETITION;

	public enum GameMode {
		COMPETITION(0), TRAINING(1), DEMO(2);

		private final byte mode;

		GameMode(int i) {
			mode = (byte) i;
		}

		public byte getValue() {
			return mode;
		}

		@Override
		public String toString() {
			return this.name();
		}

		public static GameMode fromString(String gameMode) {
			switch (gameMode) {
				case "TRAIN":
					return GameMode.TRAINING;
				case "DEMO":
					return GameMode.DEMO;
				case "COMPETITION":
					return GameMode.COMPETITION;
				default:
					throw new IllegalArgumentException("Unknown GameMode");
			}
		}
	}

	//Simulation Component Settings
	public static boolean SIMULATION_COMPONENT_ENABLED = true;
	public static boolean SIM_DEBUG_MODE = false;

	// Level Selection Settings
	public static int START_LEVEL = 1;
	public static int LEVEL_RANGE = 0;
	public static int ROUNDS = -1;

	public static boolean VISUAL_DEBUG_ENABLED = false;
	public static final boolean USE_NEW_SLING_DETECTION = true;
	public static boolean EXPORT_LEVEL_STATS = false; // for testing
	public static boolean DISABLE_LEVEL_SELECTION = false; // for testing
	public static boolean LEVEL_SELECTION_FIRST_ROUND_ITERATIVE = false; // for testing

	public static boolean FEEDBACK_ENABLED = false; // the feedback component is not yet ready enough as it has too many false positives

	// Parameters for Parabola Calculations
	public static double[] lowAngleChange;
	public static double[] lowAngleVelocity;
	public static final double[] highAngleBegin = new double[10];
	public static final double[][] highAngleChange = new double[10][3];
	public static final double[][] highAngleVelocity = new double[10][3];
	public static final double[] yellowBirdVelocity = new double[]{-0.2339, 0.3093, 3.7051};
	// yellowBirdVelocity: -0.4315 * Math.pow(x, 3) - 0.4029 * x * x + 0.3315 * x +
	// 3.7126; // more accurate, necessary?

	public static final Map<String, Integer> STRATEGY_WEIGHTS;

	public static final String PROLOG_FILE_EXTENSION = ".pl";
	public static Path PLANNER_START = null;
	public static Path PLANNER_EXECUTABLE = null;
	public static Path PLANNER_LIB_DIR = null;

	public static Path APP_HOME = null;
	public static Path TEMP_DIR = null;
	public static Path DEBUG_DIR = null;
	public static Path CONFIG_DIR = null;

	static {
		Hashtable<String, Integer> tmp = new Hashtable<>(6);
		// all the known strategies with the weights used in our decision tree in python
		tmp.put("bunker", 1);
		tmp.put("domino", 2);
		tmp.put("collapseStructure", 4);
		tmp.put("heavyObject", 10);
		tmp.put("targetPig", 5);
		tmp.put("defrost", 15);
		STRATEGY_WEIGHTS = Collections.unmodifiableMap(tmp);

		if (USE_NEW_SLING_DETECTION) {
			lowAngleChange = new double[]{-0.0230, -7.871e-4, 0.0540};
			lowAngleVelocity = new double[]{0.0473, -0.1756, 2.8654};
			highAngleBegin[BlueBird.id()] = Math.toRadians(77.226);
			highAngleBegin[RedBird.id()] = Math.toRadians(74.476);
			highAngleBegin[YellowBird.id()] = Math.toRadians(75.032);
			highAngleBegin[BlackBird.id()] = Math.toRadians(72.777);
			highAngleBegin[WhiteBird.id()] = Math.toRadians(69.365);
			highAngleChange[BlueBird.id()] = new double[]{-6.2164, 17.7277, -12.5889};
			highAngleChange[RedBird.id()] = new double[]{-6.8544, 19.1149, -13.2502};
			highAngleChange[YellowBird.id()] = new double[]{-7.1737, 20.0922, -13.9949};
			highAngleChange[BlackBird.id()] = new double[]{-10.4124, 28.4201, -19.2827};
			highAngleChange[WhiteBird.id()] = new double[]{-11.8720, 31.2441, -20.4036};
			highAngleVelocity[BlueBird.id()] = new double[]{39.1345, -119.2619, 92.3808};
			highAngleVelocity[RedBird.id()] = new double[]{42.1667, -124.9211, 93.8631};
			highAngleVelocity[YellowBird.id()] = new double[]{43.7502, -130.3646, 98.4179};
			highAngleVelocity[BlackBird.id()] = new double[]{65.4336, -186.8649, 134.5157};
			highAngleVelocity[WhiteBird.id()] = new double[]{73.2264, -199.8274, 137.3380};
		} else {
			lowAngleChange = new double[]{-0.0204, -0.0045, 0.0549};
			lowAngleVelocity = new double[]{0.0406, -0.1640, 2.8615};
			highAngleBegin[BlueBird.id()] = Math.toRadians(77.2834);
			highAngleBegin[RedBird.id()] = Math.toRadians(74.5361);
			highAngleBegin[YellowBird.id()] = Math.toRadians(75.0575);
			highAngleBegin[BlackBird.id()] = Math.toRadians(72.8029);
			highAngleBegin[WhiteBird.id()] = Math.toRadians(69.4597);
			highAngleChange[BlueBird.id()] = new double[]{-6.7165, 19.1056, -13.5384};
			highAngleChange[RedBird.id()] = new double[]{-6.6978, 18.7235, -13.0074};
			highAngleChange[YellowBird.id()] = new double[]{-6.9560, 19.5175, -13.6161};
			highAngleChange[BlackBird.id()] = new double[]{-11.4143, 31.0365, -20.9903};
			highAngleChange[WhiteBird.id()] = new double[]{-13.6359, 35.7343, -23.2604};
			highAngleVelocity[BlueBird.id()] = new double[]{42.8185, -129.3400, 99.2806};
			highAngleVelocity[RedBird.id()] = new double[]{36.6116, -110.3012, 84.2551};
			highAngleVelocity[YellowBird.id()] = new double[]{41.4429, -124.2663, 94.3941};
			highAngleVelocity[BlackBird.id()] = new double[]{76.0678, -214.6207, 152.6222};
			highAngleVelocity[WhiteBird.id()] = new double[]{78.4918, -213.2718, 145.9202};
		}
	}

	/**
	 * Load settings from properties file and cli
	 *
	 * @param args Commandline arguments
	 */
	public static void load(String... args) {

		// ---------------
		// setup cli parser

		Options options = new Options();

		Option help = Option.builder().longOpt("help").desc("Print this message").build();
		options.addOption(help);

		Option serverHostOption = new Option("h", "host", true, "hostname of the ABServer (Default: localhost)");
		serverHostOption.setArgName("hostname");
		options.addOption(serverHostOption);

		Option debug = new Option("v", "verbose", false, "Enable verbose/debug output");
		options.addOption(debug);

		Option teamID = new Option("t", "team", true, "Team ID (Default: 1)");
		teamID.setArgName("team_id");
		options.addOption(teamID);

		Option pathToSwipl = new Option("s", "swipl", true, "Path to SWIProlog (Default: swipl i.e. from PATH)");
		pathToSwipl.setArgName("path");
		options.addOption(pathToSwipl);

		Option startLevel = Option.builder().longOpt("start-level").hasArg()
				.desc("ID of level to start from. Default: 1 for first level").build();
		options.addOption(startLevel);

		Option range = Option.builder().longOpt("range").hasArg()
				.desc("Range of levels to play, starting from start-level. Default: 0 for maximum specified by server").build();
		options.addOption(range);

		Option rounds = Option.builder().longOpt("rounds").hasArg()
				.desc("Number of rounds to play (Only used when mode=generate). -1 for infinite rounds").build();
		options.addOption(rounds);

		Option mode = Option.builder().longOpt("mode").hasArg()
				.desc("Mode of execution. Options (case-insensitive): competition (default), train").argName("GAME_MODE")
				.build();
		options.addOption(mode);

		OptionGroup serverType = new OptionGroup();
		Option scienceBirds = Option.builder().longOpt("science-birds")
				.desc("Set if using ScienceBirds as Game (and the corresponding ABServer)").build();
		Option angryBirds = Option.builder().longOpt("angry-birds")
				.desc("Set if using AngryBirds as Game (and the corresponding ABServer)").build();
		serverType.addOption(scienceBirds);
		serverType.addOption(angryBirds);
		options.addOptionGroup(serverType);

		Option visualDebug = Option.builder().longOpt("visual-debug")
				.desc("Enable Visual debugging. Outputs files or displays continuous updates for some modules.").build();
		options.addOption(visualDebug);

		Option disableLevelSelection = Option.builder().longOpt("disable-level-selection")
				.desc("Disable Level Selection. Levels will be played in sequence and after last level will restart").build();
		options.addOption(disableLevelSelection);

		Option levelSelectionFirstRoundIterative = Option.builder().longOpt("ls-first-round-it")
				.desc("The first round will be run iterative and not random").build();
		options.addOption(levelSelectionFirstRoundIterative);

		Option exportLevelStats = Option.builder().longOpt("export-level-stats")
				.desc("Exports level statistics after the end of each level to ").build();
		options.addOption(exportLevelStats);

		Option disableSimComponent = Option.builder().longOpt("disable-sim")
				.desc("Disables simulation component").build();
		options.addOption(disableSimComponent);

		Option simDebug = Option.builder().longOpt("sim-debug")
				.desc("Disables simulation component").build();
		options.addOption(simDebug);

		Option nodeSelectionEpsilon = Option.builder().longOpt("node-selection-epsilon").hasArg()
				.desc("Epsilon value that determines the probability for weighted random selection of a Node").build();
		options.addOption(nodeSelectionEpsilon);

		// --------------

		Map<String, String> env = System.getenv();

		if (env.containsKey("APP_HOME") && !env.get("APP_HOME").endsWith("gradle")) {
			Settings.APP_HOME = Paths.get(env.get("APP_HOME"));
		} else {
			Settings.APP_HOME = Paths.get(System.getProperty("user.dir"));
		}

		Settings.TEMP_DIR =  Settings.APP_HOME.resolve("tmp");
		Settings.DEBUG_DIR =  Settings.APP_HOME.resolve("debug");
		Settings.CONFIG_DIR =  Settings.APP_HOME.resolve("config");
		if (!Settings.TEMP_DIR.toFile().exists()) {
			Settings.TEMP_DIR.toFile().mkdirs();
		}
		if (!Settings.DEBUG_DIR.toFile().exists()) {
			Settings.DEBUG_DIR.toFile().mkdirs();
		}
		if (!Settings.CONFIG_DIR.toFile().exists()) {
			Settings.CONFIG_DIR.toFile().mkdirs();
		}

		Settings.PLANNER_START = Settings.APP_HOME.resolve(Paths.get("planner","start.pl"));


		File propertiesFile = Settings.CONFIG_DIR.resolve("config.properties").toFile();

		if (!propertiesFile.isFile()) {
			try {
				if (propertiesFile.createNewFile() && propertiesFile.canWrite()) {
					// Create properties file with default values
					try (OutputStream output = new FileOutputStream(propertiesFile)) {

						Properties prop = new Properties();

						// set the properties value
						prop.setProperty("START_LEVEL", Integer.toString(Settings.START_LEVEL));
						prop.setProperty("LEVEL_RANGE", Integer.toString(Settings.LEVEL_RANGE));
						prop.setProperty("ROUNDS", Integer.toString(Settings.ROUNDS));
						prop.setProperty("SERVER_TYPE", Settings.SERVER_TYPE.toString());
						prop.setProperty("GAME_MODE", Settings.GAME_MODE.toString());
						prop.setProperty("PATH_TO_SWIPL", Settings.PATH_TO_SWIPL);
						prop.setProperty("HOST", Settings.SERVER_HOST);
						prop.setProperty("TEAM_ID", Integer.toString(Settings.TEAM_ID));
						prop.setProperty("VISUAL_DEBUG_ENABLED", Boolean.toString(Settings.VISUAL_DEBUG_ENABLED));
						prop.setProperty("DISABLE_LEVEL_SELECTION", Boolean.toString(Settings.DISABLE_LEVEL_SELECTION));
						prop.setProperty("LEVEL_SELECTION_FIRST_ROUND_ITERATIVE",
								Boolean.toString(Settings.LEVEL_SELECTION_FIRST_ROUND_ITERATIVE));
						prop.setProperty("EXPORT_LEVEL_STATS", Boolean.toString(Settings.EXPORT_LEVEL_STATS));

						prop.setProperty("SIMULATION_COMPONENT_ENABLED", Boolean.toString(Settings.SIMULATION_COMPONENT_ENABLED));
						prop.setProperty("SIMULATION_DEBUG_MODE", Boolean.toString(Settings.SIM_DEBUG_MODE));

						// save properties to project root folder
						prop.store(output, null);

						log.debug("Stored default properties to " + propertiesFile);

					} catch (IOException e) {
						log.error("Could not store default values to properties file", e);
					}
				}
			} catch (IOException e) {
				log.error("Could not create properties file", e);
			}
		}

		Properties appSettings = new Properties();
		// load config.properties file
		try (FileInputStream fis = new FileInputStream(propertiesFile)) {
			appSettings.load(fis);
		} catch (IOException e) {
			log.error("Could not load properties file", e);
			appSettings.clear();
		} catch (IllegalArgumentException e) {
			log.warn("Properties file could not be parsed so it will be ignored", e);
			appSettings.clear();
		}

		try {

			// Parse CommandLine Arguments
			CommandLineParser parser = new DefaultParser();
			CommandLine cli = parser.parse(options, args);

			if (cli.hasOption("help")) {
				// output help statement and exit
				HelpFormatter formatter = new HelpFormatter();
				formatter.printHelp("BamBirds.jar", options);
				System.exit(0);
			}

			// Get Options that can be set in cli or config.properties
			// cli options have priority

			if (cli.hasOption("host")) {
				Settings.SERVER_HOST = cli.getOptionValue("host", Settings.SERVER_HOST);
			} else if (env.containsKey("SERVER_HOST")) {
				Settings.SERVER_HOST = env.getOrDefault("SERVER_HOST", Settings.SERVER_HOST);
			} else if (appSettings.containsKey("HOST")) {
				Settings.SERVER_HOST = (String) appSettings.get("HOST");
			}

			if (cli.hasOption("team")) {
				Settings.TEAM_ID = Integer.parseInt(cli.getOptionValue("team", Integer.toString(Settings.TEAM_ID)));
			} else if (env.containsKey("TEAM_ID")) {
				Settings.TEAM_ID = Integer.parseInt(env.getOrDefault("TEAM_ID", Integer.toString(Settings.TEAM_ID)));
			} else if (appSettings.containsKey("TEAM_ID")) {
				Settings.TEAM_ID = Integer.parseInt((String) appSettings.get("TEAM_ID"));
			}

			if (cli.hasOption("swipl")) {
				Settings.PATH_TO_SWIPL = cli.getOptionValue("swipl", Settings.PATH_TO_SWIPL);
			} else if (appSettings.containsKey("PATH_TO_SWIPL")) {
				Settings.PATH_TO_SWIPL = (String) appSettings.get("PATH_TO_SWIPL");
			}

			if (cli.hasOption("start-level")) {
				Settings.START_LEVEL = Integer
						.parseInt(cli.getOptionValue("start-level", Integer.toString(Settings.START_LEVEL)));
			} else if (appSettings.containsKey("START_LEVEL")) {
				Settings.START_LEVEL = Integer.parseInt((String) appSettings.get("START_LEVEL"));
			}

			if (cli.hasOption("range")) {
				Settings.LEVEL_RANGE = Integer.parseInt(cli.getOptionValue("range", Integer.toString(Settings.LEVEL_RANGE)));
			} else if (appSettings.containsKey("LEVEL_RANGE")) {
				Settings.LEVEL_RANGE = Integer.parseInt((String) appSettings.get("LEVEL_RANGE"));
			}

			if (cli.hasOption("rounds")) {
				Settings.ROUNDS = Integer.parseInt(cli.getOptionValue("rounds", Integer.toString(Settings.ROUNDS)));
			} else if (appSettings.containsKey("ROUNDS")) {
				Settings.ROUNDS = Integer.parseInt((String) appSettings.get("ROUNDS"));
			}

			if (cli.hasOption("mode")) {
				String gameMode = cli.getOptionValue("mode", GameMode.COMPETITION.toString()).toUpperCase();
				try {
					Settings.GAME_MODE = GameMode.fromString(gameMode);
				} catch (IllegalArgumentException e) {
					throw new ParseException("Mode " + gameMode + " is not a valid game mode");
				}
			} else if (appSettings.containsKey("GAME_MODE")) {
				String gameMode = (String) appSettings.get("GAME_MODE");
				try {
					Settings.GAME_MODE = GameMode.fromString(gameMode);
				} catch (IllegalArgumentException e) {
					Settings.GAME_MODE = GameMode.COMPETITION;
				}
			}

			if (cli.hasOption("science-birds") || cli.hasOption("angry-birds")) {
				Settings.SERVER_TYPE = cli.hasOption("science-birds") ? ServerType.SCIENCE_BIRDS : ServerType.ANGRY_BIRDS;
			} else if (env.containsKey("SERVER_TYPE")) {
				String type = env.getOrDefault("SERVER_TYPE", ServerType.ANGRY_BIRDS.toString());
				if (type.equals(ServerType.ANGRY_BIRDS.toString()))
					Settings.SERVER_TYPE = ServerType.ANGRY_BIRDS;
				else
					Settings.SERVER_TYPE = ServerType.SCIENCE_BIRDS;
			} else if (appSettings.containsKey("SERVER_TYPE")) {
				String type = (String) appSettings.get("SERVER_TYPE");
				if (type.equals(ServerType.ANGRY_BIRDS.toString()))
					Settings.SERVER_TYPE = ServerType.ANGRY_BIRDS;
				else
					Settings.SERVER_TYPE = ServerType.SCIENCE_BIRDS;
			}

			if (cli.hasOption("visual-debug")) {
				Settings.VISUAL_DEBUG_ENABLED = true;
			} else if (appSettings.containsKey("VISUAL_DEBUG_ENABLED")) {
				Settings.VISUAL_DEBUG_ENABLED = Boolean.parseBoolean((String) appSettings.get("VISUAL_DEBUG_ENABLED"));
			}

			if (cli.hasOption("disable-level-selection")) {
				Settings.DISABLE_LEVEL_SELECTION = true;
			} else if (appSettings.containsKey("DISABLE_LEVEL_SELECTION")) {
				Settings.DISABLE_LEVEL_SELECTION = Boolean.parseBoolean((String) appSettings.get("DISABLE_LEVEL_SELECTION"));
			}

			if (cli.hasOption("ls-first-round-it")) {
				Settings.LEVEL_SELECTION_FIRST_ROUND_ITERATIVE = true;
			} else if (appSettings.containsKey("LEVEL_SELECTION_FIRST_ROUND_ITERATIVE")) {
				Settings.LEVEL_SELECTION_FIRST_ROUND_ITERATIVE = Boolean
						.parseBoolean((String) appSettings.get("LEVEL_SELECTION_FIRST_ROUND_ITERATIVE"));
			}

			if (cli.hasOption("export-level-stats")) {
				Settings.EXPORT_LEVEL_STATS = true;
			} else if (appSettings.containsKey("EXPORT_LEVEL_STATS")) {
				Settings.EXPORT_LEVEL_STATS = Boolean.parseBoolean((String) appSettings.get("EXPORT_LEVEL_STATS"));
			}

			if (cli.hasOption("disable-sim")) {
				Settings.SIMULATION_COMPONENT_ENABLED = false;
			} else if (appSettings.containsKey("SIMULATION_COMPONENT_ENABLED")) {
				Settings.SIMULATION_COMPONENT_ENABLED = Boolean.parseBoolean((String) appSettings.get("SIMULATION_COMPONENT_ENABLED"));
			}

			if (cli.hasOption("sim-debug")) {
				Settings.SIM_DEBUG_MODE = true;
			} else if (appSettings.containsKey("SIMULATION_DEBUG_MODE")) {
				Settings.SIM_DEBUG_MODE = Boolean.parseBoolean((String) appSettings.get("SIMULATION_DEBUG_MODE"));
			}


		} catch (ParseException e) {
			log.error("Could not parse CLI Arguments", e);
			System.exit(1);
		}

		log.debug("Settings successfully initialized");
	}
}