package de.uniba.sme.bambirds.common.utils;

import static de.uniba.sme.bambirds.common.objects.ab.ABType.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class that loads settings from config.properties
 * 
 * @author Brant Unger
 *
 */
public class Settings {
	private static final Logger log = LogManager.getLogger(Settings.class);

	// Connection Settings
	public static String serverHost = "localhost";
	public static int teamID = 1;
	public static String pathToSwipl = "/usr/bin/swipl";

	// Image specifications
	public static final int IMAGE_HEIGHT = 480;
	public static final int IMAGE_WIDTH = 840;

	/**
	 * 0 for AB and 1 for SB
	 */
	public static ServerType serverType = ServerType.ANGRY_BIRDS;

	public enum ServerType {
		ANGRY_BIRDS(0), 
		SCIENCE_BIRDS(1);

		private int type;
		private ServerType(int type){
			this.type = type;
		}

		@Override
		public String toString() {
			switch (type) {
				case 1:
					return "SCIENCE_BIRDS";
				case 0:
				default:
					return "ANGRY_BIRDS";
			}
		}
	}

	public static GameMode GAME_MODE = GameMode.COMPETITION;

	public enum GameMode {
		COMPETITION(0),
		TRAINING(1);

		private byte mode;

		private GameMode (int i){
			mode = (byte) i;
		}

		public byte getValue(){
			return mode;
		}

		@Override
		public String toString() {
			switch (mode) {
				case 0:
					return "COMPETITION";
				case 1:
				default:
					return "TRAINING";
			}
		}
	}

	// Level Selection Settings
	public static int startLevel = 1;
	public static int levelRange = 0;
	public static int rounds = -1;

	public static boolean DEBUG = false;
	public static boolean DEBUG_ENABLED = true;
	public static boolean USE_NEW_SLING_DETECTION = true;
	public static boolean PERFORMANCE_MEASUREMENT_ENABLED = false; // for testing
	public static boolean LEVEL_SELECTION_FIRST_ROUND_ITERATIVE = false; // for testing
	public static double[] lowAngleChange;
	public static double[] lowAngleVelocity;
	public static double[] highAngleBegin = new double[10];
	public static double[][] highAngleChange = new double[10][3];
	public static double[][] highAngleVelocity = new double[10][3];
	public static double[] yellowBirdVelocity = new double[] { -0.2339, 0.3093, 3.7051 };
	// yellowBirdVelocity: -0.4315 * Math.pow(x, 3) - 0.4029 * x * x + 0.3315 * x +
	// 3.7126; // more accurate, necessary?

	public static final Map<String, Integer> STRATEGY_WEIGHTS;

	static {
		Hashtable<String, Integer> tmp = new Hashtable<String, Integer>(6);
		// all the known strategies with the weights used in our decision tree in python
		tmp.put("bunker", 1);
		tmp.put("domino", 2);
		tmp.put("collapseStructure", 4);
		tmp.put("heavyObject", 10);
		tmp.put("targetPig", 5);
		tmp.put("defrost", 15);
		STRATEGY_WEIGHTS = Collections.unmodifiableMap(tmp);
	}

	public static final String PROLOG_FILE_EXTENSION = ".pl";
	public static final String PROLOG_FUNCTIONS = (Paths.get("Prolog/planner/old/functions.pl")).toAbsolutePath().normalize()
			.toString();
	public static final String NEW_PROLOG_FUNCTIONS = (Paths.get("Prolog/planner/ShotCandidatePlanner.pl")).toAbsolutePath()
			.normalize().toString();

	private static void loadParameters() {
		if (USE_NEW_SLING_DETECTION) {
			lowAngleChange = new double[] { -0.0230, -7.871e-4, 0.0540 };
			lowAngleVelocity = new double[] { 0.0473, -0.1756, 2.8654 };
			highAngleBegin[BlueBird.id] = Math.toRadians(77.226);
			highAngleBegin[RedBird.id] = Math.toRadians(74.476);
			highAngleBegin[YellowBird.id] = Math.toRadians(75.032);
			highAngleBegin[BlackBird.id] = Math.toRadians(72.777);
			highAngleBegin[WhiteBird.id] = Math.toRadians(69.365);
			highAngleChange[BlueBird.id] = new double[] { -6.2164, 17.7277, -12.5889 };
			highAngleChange[RedBird.id] = new double[] { -6.8544, 19.1149, -13.2502 };
			highAngleChange[YellowBird.id] = new double[] { -7.1737, 20.0922, -13.9949 };
			highAngleChange[BlackBird.id] = new double[] { -10.4124, 28.4201, -19.2827 };
			highAngleChange[WhiteBird.id] = new double[] { -11.8720, 31.2441, -20.4036 };
			highAngleVelocity[BlueBird.id] = new double[] { 39.1345, -119.2619, 92.3808 };
			highAngleVelocity[RedBird.id] = new double[] { 42.1667, -124.9211, 93.8631 };
			highAngleVelocity[YellowBird.id] = new double[] { 43.7502, -130.3646, 98.4179 };
			highAngleVelocity[BlackBird.id] = new double[] { 65.4336, -186.8649, 134.5157 };
			highAngleVelocity[WhiteBird.id] = new double[] { 73.2264, -199.8274, 137.3380 };
		} else {
			lowAngleChange = new double[] { -0.0204, -0.0045, 0.0549 };
			lowAngleVelocity = new double[] { 0.0406, -0.1640, 2.8615 };
			highAngleBegin[BlueBird.id] = Math.toRadians(77.2834);
			highAngleBegin[RedBird.id] = Math.toRadians(74.5361);
			highAngleBegin[YellowBird.id] = Math.toRadians(75.0575);
			highAngleBegin[BlackBird.id] = Math.toRadians(72.8029);
			highAngleBegin[WhiteBird.id] = Math.toRadians(69.4597);
			highAngleChange[BlueBird.id] = new double[] { -6.7165, 19.1056, -13.5384 };
			highAngleChange[RedBird.id] = new double[] { -6.6978, 18.7235, -13.0074 };
			highAngleChange[YellowBird.id] = new double[] { -6.9560, 19.5175, -13.6161 };
			highAngleChange[BlackBird.id] = new double[] { -11.4143, 31.0365, -20.9903 };
			highAngleChange[WhiteBird.id] = new double[] { -13.6359, 35.7343, -23.2604 };
			highAngleVelocity[BlueBird.id] = new double[] { 42.8185, -129.3400, 99.2806 };
			highAngleVelocity[RedBird.id] = new double[] { 36.6116, -110.3012, 84.2551 };
			highAngleVelocity[YellowBird.id] = new double[] { 41.4429, -124.2663, 94.3941 };
			highAngleVelocity[BlackBird.id] = new double[] { 76.0678, -214.6207, 152.6222 };
			highAngleVelocity[WhiteBird.id] = new double[] { 78.4918, -213.2718, 145.9202 };
		}

	}

	/**
	 * Load settings from properties file and cli
	 */
	public static void load(String[] args) {

		// ---------------
		// setup cli parser

		Options options = new Options();

		Option help = Option.builder().longOpt("help").desc("Print this message").build();
		options.addOption(help);

		Option serverHost = new Option("h", "host", true, "hostname of the ABServer (Default: localhost)");
		serverHost.setArgName("hostname");
		options.addOption(serverHost);

		Option debug = new Option("v", "verbose", false, "Enable verbose/debug output");
		options.addOption(debug);

		Option teamID = new Option("t", "team", true, "Team ID (Default: 1)");
		teamID.setArgName("team_id");
		options.addOption(teamID);

		Option pathToSwipl = new Option("p", "swipl", true, "Path to SWIProlog");
		pathToSwipl.setArgName("path");
		options.addOption(pathToSwipl);

		Option startLevel = Option.builder().longOpt("start-level").hasArg()
				.desc("ID of level to start from. Default: 1 for first level").build();
		// TODO: add more information on range
		options.addOption(startLevel);

		Option range = Option.builder().longOpt("range").hasArg()
				.desc("Range of levels to play, starting from start-level. Default: 0 for maximum specified by server").build();
		options.addOption(range);

		Option rounds = Option.builder().longOpt("rounds").hasArg()
				.desc("Number of rounds to play (Only used when mode=generate). -1 for infinite rounds").build();
		options.addOption(rounds);

		Option mode = Option.builder().longOpt("mode").hasArg().desc("Mode of execution. Options: competition (default), train")
				.argName("GAME_MODE").build();
		options.addOption(mode);

		OptionGroup serverType = new OptionGroup();
		Option scienceBirds = Option.builder().longOpt("science-birds")
				.desc("Set if using ScienceBirds as Game (and the corresponding ABServer)").build();
		Option angryBirds = Option.builder().longOpt("angry-birds")
				.desc("Set if using AngryBirds as Game (and the corresponding ABServer)").build();
		serverType.addOption(scienceBirds);
		serverType.addOption(angryBirds);

		// --------------

		File propertiesFile = new File("config.properties");

		if (!propertiesFile.isFile()) {
			try {
				propertiesFile.createNewFile();
				if (propertiesFile.canWrite()) {
					// Create properties file with default values
					try (OutputStream output = new FileOutputStream(propertiesFile)) {

						Properties prop = new Properties();

						// set the properties value
						prop.setProperty("START_LEVEL", Integer.toString(Settings.startLevel));
						prop.setProperty("LEVEL_RANGE", Integer.toString(Settings.levelRange));
						prop.setProperty("ROUNDS", Integer.toString(Settings.rounds));
						prop.setProperty("SERVER_TYPE", Settings.serverType.toString());
						prop.setProperty("GAME_MODE", Settings.GAME_MODE.toString());
						prop.setProperty("PATH_TO_SWIPL", Settings.pathToSwipl);
						prop.setProperty("HOST", Settings.serverHost);
						prop.setProperty("TEAM_ID", Integer.toString(Settings.teamID));

						// save properties to project root folder
						prop.store(output, null);

						log.info("Stored default properties to "+propertiesFile);

					} catch (IOException e) {
						log.error("Could not store default values to properties file", e);
					}
				}
			} catch (IOException e) {
				log.error("Could not create properties file", e);
				System.exit(1);
			}
		}

		Properties appSettings = new Properties();
		try (FileInputStream fis = new FileInputStream(propertiesFile)) {
			// load config.properties file
			appSettings.load(fis);

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
				Settings.serverHost = cli.getOptionValue("host", "localhost");
			} else if (appSettings.containsKey("HOST")) {
				Settings.serverHost = (String) appSettings.get("HOST");
			} else {
				Settings.serverHost = "localhost";
			}

			if (cli.hasOption("team")) {
				Settings.teamID = Integer.parseInt(cli.getOptionValue("team", "1"));
			} else if (appSettings.containsKey("TEAM_ID")) {
				Settings.teamID = Integer.parseInt((String) appSettings.get("TEAM_ID"));
			} else {
				Settings.teamID = 1;
			}

			if (cli.hasOption("swipl")) {
				Settings.pathToSwipl = cli.getOptionValue("swipl", "/usr/bin/swipl");
			} else if (appSettings.containsKey("PATH_TO_SWIPL")) {
				Settings.pathToSwipl = (String) appSettings.get("PATH_TO_SWIPL");
			} else {
				Settings.pathToSwipl = "/usr/bin/swipl";
			}


			if (cli.hasOption("start-level")) {
				Settings.startLevel = Integer.parseInt(cli.getOptionValue("start-level", "1"));
			} else if (appSettings.containsKey("START_LEVEL")) {
				Settings.startLevel = Integer.parseInt((String) appSettings.get("START_LEVEL"));
			} else {
				Settings.startLevel = 1;
			}

			if (cli.hasOption("range")) {
				Settings.levelRange = Integer.parseInt(cli.getOptionValue("range", "0"));
			} else if (appSettings.containsKey("LEVEL_RANGE")) {
				Settings.levelRange = Integer.parseInt((String) appSettings.get("LEVEL_RANGE"));
			} else {
				Settings.levelRange = -1;
			}

			if (cli.hasOption("rounds")) {
				Settings.rounds = Integer.parseInt(cli.getOptionValue("rounds", "-1"));
			} else if (appSettings.containsKey("ROUNDS")) {
				Settings.rounds = Integer.parseInt((String) appSettings.get("ROUNDS"));
			} else {
				Settings.rounds = 0;
			}

			if (cli.hasOption("mode")) {
				String gameMode = cli.getOptionValue("mode", "competition");
				switch (gameMode) {
					case "train":
						Settings.GAME_MODE = GameMode.TRAINING;
						break;
					case "competition":
						Settings.GAME_MODE = GameMode.COMPETITION;
						break;
					default:
						throw new ParseException("Mode " + gameMode + " is not a valid game mode");
				}
			} else if (appSettings.containsKey("GAME_MODE")) {
				String gameMode = (String) appSettings.get("GAME_MODE");
				if (gameMode.equals(GameMode.TRAINING.toString()))
					Settings.GAME_MODE = GameMode.TRAINING;
				else
					Settings.GAME_MODE = GameMode.COMPETITION;
			} else {
				Settings.GAME_MODE = GameMode.COMPETITION;
			}

			if (cli.hasOption("science-birds") || cli.hasOption("angry-birds")) {
				Settings.serverType = cli.hasOption("angry-birds") ? ServerType.ANGRY_BIRDS : ServerType.SCIENCE_BIRDS;
			} else if (appSettings.containsKey("SERVER_TYPE")) {
				String type = (String) appSettings.get("SERVER_TYPE");
				if (type.equals(ServerType.ANGRY_BIRDS.toString()))
					Settings.serverType = ServerType.ANGRY_BIRDS;
				else
					Settings.serverType = ServerType.SCIENCE_BIRDS;
			} else {
				Settings.serverType = ServerType.ANGRY_BIRDS;
			}

		} catch (IOException e) {
			log.error("Could not load properties file", e);
			System.exit(1);
		} catch (ParseException e) {
			log.error("Could not parse CLI Arguments", e);
			System.exit(1);
		}

		loadParameters();

		log.info("Settings successfully initialized");
	}
}