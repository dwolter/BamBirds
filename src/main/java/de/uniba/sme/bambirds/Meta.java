package de.uniba.sme.bambirds;

import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.vision.GameStateExtractor;
import de.uniba.sme.bambirds.vision.Vision;
import de.uniba.sme.bambirds.vision.VisionHelper;
import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.database.LevelStorage;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.Target;
import de.uniba.sme.bambirds.level_selection.LevelSelection;
import de.uniba.sme.bambirds.execution.ShotExecutor;
import de.uniba.sme.bambirds.common.utils.SWIConnector;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.knowledge.Knowledge;
import de.uniba.sme.bambirds.planner.NewPrologPlanParser;
import de.uniba.sme.bambirds.planner.PrologPlanner;

import java.awt.Point;
import java.awt.image.BufferedImage;
import java.nio.file.Path;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Meta implements ShotExecutor.ShotExecutorCallback {
	private static final Logger log = LogManager.getLogger(Meta.class);

	static public boolean EVALUATE_SHOTS = true;

	private LevelSelection levelSelector;
	private SWIConnector connector;
	private Thread connectorThread;
	private GameStateExtractor extractor;
	private ShotExecutor shotExecutor = null;

	// state machine and shot evaluation
	private boolean levelInitialized = false;
	private Level currentLevel = null;
	private int previousScore = 0;
	private int lastPigCount = 0;
	private Shot lastShot = null;
	public Target lastTarget = null;
	public String lastShotDescriptor = "";
	public boolean isPlaying = true;

	public Meta(String pathToSwipl, LevelSelection levelSelector) {
		this.levelSelector = levelSelector;
		this.connector = new SWIConnector(pathToSwipl, Settings.NEW_PROLOG_FUNCTIONS);
		connectorThread = new Thread(connector);
		this.extractor = new GameStateExtractor();
	}

	/**
	 * Callback method for prolog file generation
	 * @param level
	 * @param basename
	 * @return
	 * @throws IllegalArgumentException
	 */
	public List<Target> prologGetTargets(Level level, String basename) throws IllegalArgumentException {
		Path file = new Knowledge().buildModel(level, basename);
		PrologPlanner planner = new PrologPlanner(connector, file.toString(), new NewPrologPlanParser());
		return planner.planSynchronously(20000); // 20 seconds at most for planning...
	}

	private void selectNextLevel() throws ServerException {
		selectNextLevel(null);
	}

	/**
	 * Tell server to load a different level and update current level object
	 */
	private void selectNextLevel(Level level) throws ServerException {
		levelInitialized = true;
		log.info("Choosing new level ...");
		int lvl = 0;
		if (level == null) {
			lvl = levelSelector.selectNextLevel();
		} else {
			lvl = level.levelId;
		}
		if (lvl == -1) {
			log.info("Received level ID \"" + lvl + "\", will shut down...");
			isPlaying = false;
			return;
		}
		log.info("Loading Level " + lvl);
		if (currentLevel != null && currentLevel.levelId == lvl) {
			Client.get().restartLevel();
		} else {
			Client.get().loadLevel(lvl);
		}

		this.levelSelector.increaseLevelIteration();

		// get level object from memory or create fresh one
		currentLevel = LevelStorage.getInstance().getLevelById(lvl);
		if (currentLevel == null) {
			currentLevel = new Level(lvl);
		}

		// to be able to measure time it took for level to finish
		currentLevel.updateStartTimestamp();

		currentLevel.resetToUnloadedState();
		shotExecutor = new ShotExecutor(currentLevel, this::prologGetTargets, this);
		previousScore = currentLevel.getBestScore();

		try {
			Thread.sleep(100);
		} catch (InterruptedException ignored) {
		}
	}

	/**
	 * @return Returns true if Meta has just initialised or {@code shotCount > 10}
	 */
	private boolean shouldStartNextLevel() {
		if (!levelInitialized)
			return true; // game has just started or crashed

		if (shotExecutor.currentShot > 10) { // if vision fails and agent keeps clicking somewhere on the screen
			currentLevel.setDangerous(true);
			log.info("Number of executed Shots exceeded max shots");
			currentLevel.finishLevel(GameState.LOST, 0);
			currentLevel.ditchInitialScene();
			LevelStorage.getInstance().addLeveltoStorage(currentLevel);
			return true;
		}
		return false;
	}

	public void startMeta() throws InterruptedException, ServerException {
		connectorThread.start();

		BufferedImage scr;

		while (Client.get().getState() != -1 && isPlaying) { // terminate if signal byte -1 received, means competition is
																													// over
			GameState state = Client.get().getGameState(); // one of: UNKNOWN, MAIN_MENU, EPISODE_MENU, LEVEL_SELECTION,
																										// LOADING, PLAYING, WON, LOST
			log.debug("Current GameState is "+state);
			scr = null; // forget previous screenshot
			switch (state) {
				case PLAYING:
					if (shouldStartNextLevel()) {
						selectNextLevel();
						break;
					}

					if (currentLevel.safetyMeasuresEnabled()) {
						log.info("Waiting till Scene is really stable because safety measures are enabled");
						ShotExecutor.waitTillSceneIsStableNoListener();
					}
					try {
						scr = Client.get().doScreenShot();
						evaluatePreviousShot(scr);
						if (!shotExecutor.run(scr)) {
							currentLevel.setDangerous(true);
							log.error("OUCH: shot planning or shooting failed, trying another level");
							currentLevel.finishLevel(GameState.LOST, 0);
							LevelStorage.getInstance().addLeveltoStorage(currentLevel);
							selectNextLevel();
						}
					} catch (NullPointerException e) {
						currentLevel.setDangerous(true);
						log.error(" OUCH: Got a NullPointerException while playing, trying another level", e);
						currentLevel.finishLevel(GameState.LOST, 0);
						LevelStorage.getInstance().addLeveltoStorage(currentLevel);
						selectNextLevel();
					}
					break;

				// at end of level record
				case LOST:
					if (levelInitialized)
						currentLevel.numOfBirdsConfident = currentLevel.numOfBirds == currentLevel.executedShots.size();
					// and also everything for WON ...
				case WON:
					if (levelInitialized) {
						scr = Client.get().doScreenShot();
						int endScore = 0;
						if (state == GameState.WON) {
							endScore = extractor.getScoreEndGame(scr);
						}

						currentLevel.finishLevel(state, endScore);
						if (EVALUATE_SHOTS) {
							log.info(String.format("%s score: %d killed: %d", lastShotDescriptor, (endScore - previousScore),
									lastPigCount));
						}

						// timestamp is in milliseconds, convert to seconds
						this.levelSelector.decreaseRemainingTime(currentLevel.getCosts());

						log.info("Level Selection: Remaining Time " + this.levelSelector.getRemainingTime() + ", current Iteration "
								+ this.levelSelector.getCurrentIteration());

						LevelStorage.getInstance().addLeveltoStorage(currentLevel);
					}
					selectNextLevel();
					break;
				case LOADING:
					Thread.sleep(1000);
					break;
				case LEVEL_SELECTION:
				case MAIN_MENU:
				case EPISODE_MENU:
					selectNextLevel();
					break;
				default:
					log.error("unhandled game state, start game and navigate to level selection screen!");
					Thread.sleep(1000);
					break;
			}
		}
	}

	/**
	 * Calculate score difference to previous screen
	 * 
	 * @param img Reusing screenshot from other analysis
	 */
	private void evaluatePreviousShot(BufferedImage img) {
		int newScore = 0;
		if (!shotExecutor.isFirstShot()) { // record score of previous shot
			newScore = extractor.getScoreInGame(img);
			currentLevel.addExecutedShot(lastShot, lastTarget, newScore - previousScore); // record score ...
			previousScore = newScore;
		}
		if (EVALUATE_SHOTS) {
			Vision v = new Vision(img);
			int pigsRemaining = v.findPigsRealShape().size();
			if (!shotExecutor.isFirstShot()) {
				// log.info("Sl: pigsRemaining=" + pigsRemaining + ", lastPigCount=" +
				// lastPigCount);
				log.info(String.format("%s score: %d killed: %d", lastShotDescriptor, (newScore - previousScore),
						(lastPigCount - pigsRemaining)));
			}
			lastPigCount = pigsRemaining;
		}
	}

	/**
	 * @return false if timeout exceeded
	 * @throws ServerException
	 */
	private boolean waitForLevelEnd() throws ServerException {
		long future = System.currentTimeMillis() + 6000; // wait max 6s
		while (System.currentTimeMillis() < future) {
			log.info("checking state: " + (future - System.currentTimeMillis()));
			switch (Client.get().getGameState()) {
				case WON:
					try {
						Thread.sleep(3000);
					} catch (Exception ignored) {
					}
				case LOST:
					return true;
				case PLAYING:
					break;
				default:
					log.info("Unexpected GameState");
			}
			try {
				Thread.sleep(100);
			} catch (Exception ignored) {
			}
		}
		return false;
	}

	public void shutdown() {
		log.info("Shutting down Meta");
		shotExecutor.shutdown();
		connector.shutdown();
		connectorThread.interrupt();
	}

	/**
	 * @return Return value for {@link ShotExecutor#run(BufferedImage)} command
	 * @throws ServerException
	 */
	@Override
	public boolean sceneInitialisationFailed(BufferedImage img, SceneInitialisationException e) throws ServerException {
		currentLevel.setDangerous(true);
		log.error("Scene detection failed: " + e.getMessage());
		return waitForLevelEnd();
	}

	/**
	 * Just before execution
	 */
	@Override
	public void shotSelected(Shot proposedShot, Target target, boolean demoShot) {
		if (EVALUATE_SHOTS) {
			lastTarget = target; // null in case of demo shot
			if (demoShot)
				lastShotDescriptor = currentLevel.currentScene + " Ta:(naive)";
			else
				lastShotDescriptor = currentLevel.currentScene + " Ta:(" + lastTarget + ")";
		}
	}

	/**
	 * Evaluated predicted parabola with actual and update decision tree
	 * 
	 * @throws ServerException
	 */
	@Override
	public void shotDelivered(Shot shot) throws ServerException {
		lastShot = shot;
		BufferedImage img = VisionHelper.screenshotWithoutMovingParts(Client.get().doScreenShot());
		

		// Step 1: Evaluate parabola (bird has just landed or is lying on the ground
		// already)
		// Evaluate trajectory after shot only for unaltered parabolas
		Point releasePoint = new Point(shot.getDx(), shot.getDy());
		double newScale = VisionHelper.calculateScalingFactor(img, currentLevel, releasePoint, shot.getT_tap());
		// FIXME: check if slingshot detection failed for first level, otherwise any
		// call to this level will fail
		currentLevel.setSlingshotAndScaling(null, newScale);

		// Step 2: Set decision tree image
		// compare current scene with scene of node we should currently be in
		// if different: create new node with current 'scene'
		currentLevel.tree.getCurrentNode().setShot(shot);
		currentLevel.tree.compareScreenshotsReplaceCurrentNode(img);
		currentLevel.tree.getCurrentNode().savePoints(currentLevel.currentScene.sortedTargetsList());
		currentLevel.tree.getCurrentNode().compareCurrentSituationWithPrevSituation();
	}

	/**
	 * Will be called after everything has come to a still (max 16s)
	 * 
	 * @param moving Hard limit exceeded
	 * @throws ServerException
	 */
	@Override
	public void shotDeliveredSceneStable(BufferedImage img, boolean moving, boolean noBirds, boolean noPigs)
			throws ServerException {

		if (extractor.getGameState(img) != GameState.LOST) {
			int currentScore = this.extractor.getScoreInGame(img);

			// update current score of level every time scene is stable
			this.currentLevel.currentScore = currentScore;
		}

		if (noBirds || noPigs)
			waitForLevelEnd();
		// seems like we won the level, wait for server to record
		else if (extractor.getGameState(img) != GameState.PLAYING)
			try {
				Thread.sleep(100);
			} catch (Exception ignored) {
			}
	}
}
