package de.uniba.sme.bambirds;

import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.vision.GameStateExtractor;
import de.uniba.sme.bambirds.vision.Vision;
import de.uniba.sme.bambirds.vision.VisionHelper;
import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.Strategy;
import de.uniba.sme.bambirds.common.database.LevelStorage;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.level_selection.LevelSelection;
import de.uniba.sme.bambirds.execution.ShotExecutor;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.PrologPlanParser;
import de.uniba.sme.bambirds.planner.PrologPlanner;

import java.awt.Point;
import java.awt.image.BufferedImage;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Meta implements ShotExecutor.ShotExecutorCallback, ShotExecutor.StrategyCallback {
	private static final Logger log = LogManager.getLogger(Meta.class);

	static public boolean EVALUATE_SHOTS = true;

	private LevelSelection levelSelector;
	private GameStateExtractor extractor;
	private ShotExecutor shotExecutor = null;

	// state machine and shot evaluation
	private boolean levelInitialized = false;
	private Level currentLevel = null;
	private int previousScore = 0;
	private int lastPigCount = 0;
	private Shot lastShot = null;
	public Plan lastPlan = null;
	public String lastShotDescriptor = "";
	public boolean isPlaying = true;

	public Meta(LevelSelection levelSelector) {
		this.levelSelector = levelSelector;
		this.extractor = new GameStateExtractor();
	}

	/**
	 * Callback method for prolog file generation
	 *
	 * @param level
	 * @return
	 * @throws IllegalArgumentException
	 */
	@Override
	public Strategy getStrategy(Level level, int currentShot) throws IllegalArgumentException {
		String basename = "situation" + level.levelId + "-" + currentShot;
		PrologPlanner planner = new PrologPlanner(level, basename, new PrologPlanParser());
		return planner;
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
		if (shotExecutor != null){
			shotExecutor.shutdown();
		}
		shotExecutor = new ShotExecutor(currentLevel, this, this);
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
			log.warn("Number of executed Shots exceeded max shots");
			currentLevel.finishLevel(GameState.LOST, 0);
			currentLevel.ditchInitialScene();
			LevelStorage.getInstance().addLeveltoStorage(currentLevel);
			return true;
		}
		return false;
	}

	public void startMeta() throws InterruptedException, ServerException {

		BufferedImage scr;

		while (Client.get().getState() != -1 && isPlaying) { // terminate if signal byte -1 received, means competition is
																													// over
			GameState state = Client.get().getGameState(); // one of: UNKNOWN, MAIN_MENU, EPISODE_MENU, LEVEL_SELECTION,
																											// LOADING, PLAYING, WON, LOST
			log.debug("Current GameState is " + state);
			scr = null; // forget previous screenshot
			switch (state) {
				case PLAYING:
					if (shouldStartNextLevel()) {
						selectNextLevel();
						break;
					}

					if (currentLevel.safetyMeasuresEnabled()) {
						log.info("Waiting till Scene is really stable because safety measures are enabled");
						ShotExecutor.waitTillSceneIsStableNoListener(currentLevel);
					}
					try {
						scr = Client.get().doScreenShot();
						evaluatePreviousShot(scr);
						if (!shotExecutor.run(scr)) {
							currentLevel.setDangerous(true);
							currentLevel.ditchInitialScene();
							log.error("OUCH: shot planning or shooting failed, trying another level");
							currentLevel.finishLevel(GameState.LOST, 0);
							LevelStorage.getInstance().addLeveltoStorage(currentLevel);
							selectNextLevel();
						}
					} catch (NullPointerException e) {
						currentLevel.setDangerous(true);
						currentLevel.ditchInitialScene();
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
							if (Settings.SERVER_TYPE == Settings.ServerType.SCIENCE_BIRDS) {
								endScore = Client.get().getCurrentLevelScoreInt();
							} else {
								endScore = extractor.getScoreEndGame(scr);
							}
						}

						currentLevel.finishLevel(state, endScore);
						if (EVALUATE_SHOTS) {
							log.debug(String.format("%s score: %d killed: %d", lastShotDescriptor, (endScore - previousScore),
									lastPigCount));
						}

						// timestamp is in milliseconds, convert to seconds
						this.levelSelector.decreaseRemainingTime(currentLevel.getCosts());

						log.debug("Remaining Time " + this.levelSelector.getRemainingTime() + ", current Iteration "
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
				case NEWTESTSET:
				case NEWTRAININGSET:
				case NEWTRIAL:
					Client.get().readyForNewSet();
					break;
				case REQUESTNOVELTYLIKELIHOOD:
					// TODO: these are just the default values from the provided code
					Client.get().reportNoveltyLikelihood(0.9f,0.1f, new int[]{1,-2,-398879789}, 0, "");
					break;
				case EVALUATION_TERMINATED:
					log.error("Evaluation terminated, exiting ...");
					this.isPlaying = false;
				case RESUMETRAINING:
				default:
					log.error("unhandled game state {}, start game and navigate to level selection screen!", state);
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
	private void evaluatePreviousShot(BufferedImage img) throws ServerException {
		int newScore = 0;
		if (!shotExecutor.isFirstShot()) { // record score of previous shot
			newScore = Settings.SERVER_TYPE == Settings.ServerType.SCIENCE_BIRDS ?
					Client.get().getCurrentLevelScoreInt()
					: this.extractor.getScoreInGame(img);
			currentLevel.addExecutedShot(lastShot, lastPlan, newScore - previousScore); // record score ...
			// Reset FailedShots if a score was achieved
			if (newScore - previousScore > 0)
				currentLevel.numFailedShots = 0;
			previousScore = newScore;
		}
		if (EVALUATE_SHOTS) {
			Vision v = new Vision(img);
			int pigsRemaining = v.findPigsRealShape().size();
			if (!shotExecutor.isFirstShot()) {
				log.info("{} score: {} killed: {}", lastShotDescriptor, (newScore - previousScore),
						(lastPigCount - pigsRemaining));
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
			log.debug("checking state: " + (future - System.currentTimeMillis()));
			GameState state = Client.get().getGameState();
			switch (state) {
				case WON:
					try {
						Thread.sleep(3000);
					} catch (Exception ignored) {
					}
				case NEWTRIAL:
				case NEWTESTSET:
				case NEWTRAININGSET:
				case REQUESTNOVELTYLIKELIHOOD:
				case LOST:
					return true;
				case PLAYING:
					break;
				default:
					log.warn("Unexpected GameState {}", state);
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
		if (shotExecutor != null) {
			shotExecutor.shutdown();
		}
	}

	/**
	 * @return Return value for {@link ShotExecutor#run(BufferedImage)} command
	 * @throws ServerException
	 */
	@Override
	public boolean sceneInitialisationFailed(BufferedImage img, SceneInitialisationException e) throws ServerException {
		currentLevel.setDangerous(true);
		ImageUtil.saveToDebugFile(img, "scene_init_failed-" + currentLevel.levelId + "-" + shotExecutor.currentShot);
		log.error("Scene detection failed: " + e.getMessage());
		return waitForLevelEnd();
	}

	/**
	 * Just before execution
	 */
	@Override
	public void shotSelected(Shot proposedShot, Plan plan, boolean demoShot) {
		if (EVALUATE_SHOTS) {
			lastPlan = plan; // null in case of demo shot
			if (demoShot)
				lastShotDescriptor = currentLevel.currentScene + " Ta:(naive)";
			else
				lastShotDescriptor = currentLevel.currentScene + " Ta:(" + lastPlan + ")";
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
		BufferedImage img = Client.get().doScreenShot();

		// Step 1: Evaluate parabola (bird has just landed or is lying on the ground
		// already)
		// Evaluate trajectory after shot only for unaltered parabolas
		Point releasePoint = new Point(shot.getDragX(), shot.getDragY());
		double newScale = VisionHelper.calculateScalingFactor(img, currentLevel, releasePoint, shot.getTapTime());
		double currentScale = currentLevel.getScalingFactor();
		// FIXME: check if slingshot detection failed for first level, otherwise any
		// call to this level will fail
		currentLevel.setSlingshotAndScaling(null, newScale);

		// Update Nodes because of scaling factor change
		// Otherwise all shots would miss
		if (Math.abs(currentLevel.getScalingFactor()-currentScale) > 1e-3) {
			currentLevel.tree.adaptNodesToNewScalingFactor(currentScale, currentLevel.getScalingFactor());
		}

		// Step 2: Set decision tree image
		// compare current scene with scene of node we should currently be in
		// if different: create new node with current 'scene'
//		currentLevel.tree.getCurrentNode().setShot(shot);
		currentLevel.tree.compareScreenshotsReplaceCurrentNode(img);
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

		if ((Settings.SERVER_TYPE == Settings.ServerType.SCIENCE_BIRDS && Client.get().getGameState() != GameState.LOST)
				|| (Settings.SERVER_TYPE == Settings.ServerType.ANGRY_BIRDS && extractor.getGameState(img) != GameState.LOST)) {

			// update current score of level every time scene is stable
			this.currentLevel.currentScore = Settings.SERVER_TYPE == Settings.ServerType.SCIENCE_BIRDS ?
					Client.get().getCurrentLevelScoreInt()
					: this.extractor.getScoreInGame(img);
		}

		if (noBirds || noPigs)
			waitForLevelEnd();
		// seems like we won the level, wait for server to record
		else if ((Settings.SERVER_TYPE == Settings.ServerType.SCIENCE_BIRDS && Client.get().getGameState() != GameState.PLAYING)
				|| (Settings.SERVER_TYPE == Settings.ServerType.ANGRY_BIRDS && extractor.getGameState(img) != GameState.PLAYING))
			try {
				Thread.sleep(100);
			} catch (Exception ignored) {
			}
	}
}
