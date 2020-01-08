package main;

import ab.demo.other.ClientActionRobot;
import features.VisualDebugger;
import helper.CustomLogger;
import helper.SrcFileCopy;
import meta.ActionRobot;
import level_selection.LevelSelection;
import meta.Meta;

import java.util.Locale;

import static helper.Constants.DEBUG_ENABLED;


public class BamBird {
	private int numOfLevels;
	private LevelSelection levelSelector;
	/** The ongoing round in the competition. */
	private int roundInfo;
	/** The time limit in minutes. */
	private int timeLimit;

	private static BamBird instance = null;
	public static String serverHost = "";
	private static int team_id = 424242;
	private Meta meta;

	private BamBird() { }

	public static BamBird getInstance() {
		if (instance == null) {
			instance = new BamBird();
		}
		return instance;
	}

	private void start(String pathToSwipl, int start_level, int rounds, int timeLimit, int range) {
		try {
			levelSelector = new LevelSelection(numOfLevels, start_level, rounds, timeLimit, range);

			//LevelStorage.getInstance().restoreFromFile(); // in case agent crashed
			meta = new Meta(pathToSwipl, levelSelector);
			meta.startMeta();

		} catch (InterruptedException e) {
			e.printStackTrace();
		} finally {
			meta.shutdown();
			//LevelStorage.getInstance().storeToFile();
		}
	}

	private boolean isPlaying(){
		if (meta != null) {
			return meta.isPlaying;
		} else {
			return true;
		}
	}

	public static void main(String[] args) {
		// sets the language for the logger to "english" -- can be disabled
		Locale.setDefault(new Locale("en", "EN"));
		// if Constants.DEBUG_ENABLED=true, logger will log everything. Else, logger will only log "severe" messages.
		if (DEBUG_ENABLED){
			CustomLogger.setLogLevel(CustomLogger.LogLevel.INFO);
		} else {
			CustomLogger.severe("Started agent without info logging, only severe errors will be printed. To enable logging, change DEBUG_MODE in Constants.class.");
		}

		if (!VisualDebugger.globalDebuggingEnabled)
        	System.setProperty("java.awt.headless", "true"); // surpress menu bar
		if (args.length < 3) {
			CustomLogger.severe("Please provide valid arguments: server_host, team_id, path_to_swi-prolog, [start level, rounds, number of levels]");
			return;
		}

		serverHost = args[0];
		team_id = new Integer(args[1]);
		int start_level = 1;
		int rounds = -1;
		int range = 0;
		if (args.length > 3) {
			try {
				start_level = Integer.parseInt(args[3]);
				if (args.length > 4){
					rounds = Integer.parseInt(args[4]);
				}
				if (args.length > 5){
					range = Integer.parseInt(args[5]);
				}
			} catch (NumberFormatException e) {
				CustomLogger.warning("couldn't parse start level, round number or level range, ignoring.");
			}
		}

		CustomLogger.info("Version 2019-08-13"); // FIXME: pull version from git

		SrcFileCopy.init(); // copy Prolog folder outside of jar

		BamBird bamBird = BamBird.getInstance();
		byte[] info = ActionRobot.get().configure(ClientActionRobot.intToByteArray(team_id));
		bamBird.roundInfo = info[0];
		bamBird.timeLimit = info[1];
		bamBird.numOfLevels = info[2]; // FIXME: should exploit other info from ABServer too

		CustomLogger.info("roundInfo: " + bamBird.roundInfo + ", timeLimit: " + bamBird.timeLimit + ", numOfLevels: " + bamBird.numOfLevels + ", starting level: "+start_level+", range: "+range);

		while (ActionRobot.get().getState() != -1 && bamBird.isPlaying()) { // shut down if competition is over, all requests to the server should then return -1
			try {
				bamBird.start(args[2], start_level, rounds, bamBird.timeLimit, range);
			} catch (Exception e) {
				// Never crash for any reason, just retry forever
				CustomLogger.severe("A fatal error occurred:");
				e.printStackTrace();
				CustomLogger.severe(String.format("%n%nRestarting BamBird...%n%n"));
			}
		}
		CustomLogger.severe("Shutting down BamBird");
	}
}
