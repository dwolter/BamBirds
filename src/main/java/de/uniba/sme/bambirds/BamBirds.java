package de.uniba.sme.bambirds;

import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.BamBirdModule;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.utils.ConfigureResponse;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.PrologPlanner;
import de.uniba.sme.bambirds.level_selection.LevelSelection;

import java.util.Locale;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public final class BamBirds extends BamBirdModule {
	private static final Logger LOG = LogManager.getLogger(BamBirds.class);

	private int numOfLevels;
	private LevelSelection levelSelector;
	/** The ongoing round in the competition. */
	private ConfigureResponse.RoundType roundInfo;
	/** The time limit in minutes. */
	private int timeLimit;

	private static BamBirds instance = null;
	private Meta meta;

	private BamBirds() { }

	public static BamBirds getInstance() {
		if (instance == null) {
			instance = new BamBirds();
		}
		return instance;
	}

	private void start() throws ServerException {
		try {
			levelSelector = new LevelSelection(numOfLevels, timeLimit);

			// LevelStorage.getInstance().restoreFromFile(); // in case agent crashed
			LOG.info("Starting Meta");
			meta = new Meta(levelSelector);
			meta.startMeta();

		} catch (InterruptedException e) {
			LOG.error("Meta got interrupted", e);
		} finally {
			shutdown();
			// LevelStorage.getInstance().storeToFile();
		}
	}

	private boolean isPlaying() {
		if (meta != null) {
			return meta.isPlaying;
		} else {
			return true;
		}
	}

	public static void main(String[] args) {
		// sets the language for the logger to "english" -- can be disabled
		Locale.setDefault(new Locale("en", "EN"));

		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			if (instance != null) {
				instance.shutdown();
				Client.shutdown();
			}
		}));

		Settings.load(args);

		// if Constants.DEBUG_ENABLED=true, logger will log everything. Else, logger
		// will only log "severe" messages.
		// if (DEBUG_ENABLED){
		// log.setLogLevel(log.LogLevel.INFO);
		// } else {
		// log.severe("Started agent without info logging, only severe errors will be
		// printed. To enable logging, change DEBUG_MODE in Constants.class.");
		// }

		// TODO: do this somewhere else
		// if (!VisualDebugger.globalDebuggingEnabled)
		// System.setProperty("java.awt.headless", "true"); // surpress menu bar
		String version = BamBirds.class.getPackage().getImplementationVersion();
		if (version == null) {
			version = "current";
		}
		LOG.always().log("Version {}", version);

		LOG.info("Connecting to ABServer");
		int tries = 0;
		while (true) {
			try {
				tries++;
				Client.init();
				break;
			} catch (ServerException e) {
				LOG.error("Could not connect to ABServer, retrying...", e);
				try {
					TimeUnit.SECONDS.sleep(1);
				} catch (InterruptedException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				if (tries >= 10) {
					System.exit(1);
				}
			}
		}

		PrologPlanner.compileExecutable();

		BamBirds bamBirds = BamBirds.getInstance();

		ConfigureResponse info = new ConfigureResponse(0, -1, 21);
		try {
			info = Client.get().configure(Settings.TEAM_ID);
		} catch (ServerException e) {
			LOG.fatal("Could not register at Server", e);
			System.exit(1);
		}
		bamBirds.roundInfo = info.getRound();
		bamBirds.timeLimit = info.getTimeLimit();
		bamBirds.numOfLevels = info.getNumberOfLevels(); // FIXME: should exploit other info from ABServer too

		LOG.debug("Server response: roundInfo = " + bamBirds.roundInfo + ", timeLimit = " + bamBirds.timeLimit + ", numOfLevels = " + bamBirds.numOfLevels);
		try {
			while (bamBirds.isPlaying()) { // shut down if competition is over, all requests to the server should then return -1
				try {
					bamBirds.start();
				} catch (ServerException e) {
					LOG.fatal(e.getMessage(), e);
					break;
				} catch (Exception e) {
					// Never crash for any reason, just retry forever
					LOG.error("A fatal error occurred", e);
//					log.error("Restarting BamBirds...");
					throw e;
				}
			}
		} finally {
			LOG.warn("Shutting down BamBirds");
			// No need to shutdown here, since that is handled by the ShutdownHook
		}
	}

	@Override
	public void shutdown() {
		if (meta != null) {
			LOG.info("Shutting down Meta");
			meta.shutdown();
		}
	}
}
