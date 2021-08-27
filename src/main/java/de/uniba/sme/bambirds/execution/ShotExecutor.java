package de.uniba.sme.bambirds.execution;

import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.knowledge.Knowledge;
import de.uniba.sme.bambirds.vision.Vision;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.SimpleFuture;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;
import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.BamBirdModule;
import de.uniba.sme.bambirds.common.Strategy;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.AbstractScene;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.vision.SaveScreenshots;
import de.uniba.sme.bambirds.vision.Scene;
import de.uniba.sme.bambirds.vision.VisionHelper;
import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.objects.Node;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.planner.shot.ShotSelection;

import de.uniba.sme.bambirds.vision.GameStateExtractor;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.imageio.ImageIO;

public class ShotExecutor extends BamBirdModule {
	private static final Logger log = LogManager.getLogger(ShotExecutor.class);

	static private VisualDebugger DBG = new VisualDebugger("ShotExecutor");
	{ DBG.enableDebug(false, true); }

	public enum SceneLoaded {
		NOT_LOADED, NEW, FROM_MEM;
	}

	public interface StrategyCallback {
		Strategy getStrategy(Level level, int currentShot) throws IllegalArgumentException;
	}

	public interface ShotExecutorCallback {
		boolean sceneInitialisationFailed(BufferedImage screenshot, SceneInitialisationException exception)
				throws ServerException;

		void shotSelected(Shot proposedShot, Plan plan, boolean demoShot);

		void shotDelivered(Shot shot) throws ServerException;

		void shotDeliveredSceneStable(BufferedImage stableScreen, boolean stillMoving, boolean noBirds, boolean noPigs)
				throws ServerException;
	}

	// Constant variables
	static private long SHOT_DELAY = 5000; // min time to wait, takes ~5s for birds to fly the longest parabola

	// instance variables
	private Level currentLevel;
	private StrategyCallback strategyCallback;
	private ShotExecutorCallback listener;
	private ExecutorService pool;
	public int currentShot = 1;

	public ShotExecutor(Level level, StrategyCallback callback, ShotExecutorCallback listener) {
		this.currentLevel = level;
		this.strategyCallback = callback;
		this.listener = listener;
		this.pool = Executors.newCachedThreadPool();
		DBG.enableDebug(Settings.GAME_MODE == Settings.GameMode.DEMO, Settings.VISUAL_DEBUG_ENABLED);
	}

	public boolean isLastShot(Vision v) {
		// If we had not more shots than the number of Birds it is not the last shot
		if (currentLevel.numOfBirds != currentShot)
			return false;
		// It is the last shot if there are no birds or the found bird is not in the slingshot
		List<ABObject> birds = v.findBirdsRealShape();
		return birds == null || birds.size() == 0 || birds.get(0).getCenter().distance(currentLevel.getSlingshot().getPivotPoint2D()) > 50;
	}

	public boolean isFirstShot() {
		return (currentShot == 1);
	}

	/**
	 * Do everything. From gathering the current scene, to Prolog planning, Shot
	 * selection, Shot execution and after-shot evaluation
	 *
	 * @param img Reusing screenshot from other analysis
	 * @return Returns true if shot was delivered successfully
	 * @throws ServerException
	 */
	public boolean run(BufferedImage img) throws ServerException {
		ShotSelection shotSelector;
		List<Plan> plans;

		SceneLoaded loadedFrom = SceneLoaded.NOT_LOADED;
		try {
			loadedFrom = prepareCurrentScene(img);
		} catch (SceneInitialisationException e) {
			return listener.sceneInitialisationFailed(img, e);
		}
		if (loadedFrom == SceneLoaded.NOT_LOADED)
			return false;

		// FIXME: For some reason there is a NullPointerException thrown here in some circumstances.
		// Don't really know why and it is also not really reproducible as it never happens when debugging
		try {
			// If the bird count equals the count in the previous node it is safe to assume that there was no bird shot
			if (currentLevel.tree.getCurrentNode().parent != null &&
					currentLevel.tree.getCurrentNode().parent.getScene() != null &&
					currentLevel.currentScene.getBirds().size() == currentLevel.tree.getCurrentNode().parent.getScene().getBirds().size())
				currentLevel.numFailedShots++;
		} catch (NullPointerException ignored) {  }

		// Executing fails if there were at least 3 failed consecutive shots.
		// Then the initial scene is also ditched and safety measures enabled
		if (currentLevel.numFailedShots >= 3)
			return false;

		boolean success = true;

		shotSelector = new ShotSelection(currentLevel, currentShot - 1);
		if (Settings.STRATEGY_ASYNC) {
			Future<Boolean> f = generateTargets(loadedFrom);
			try {
				if (!f.get(Settings.STRATEGY_TIMEOUT, Settings.STRATEGY_TIMEOUT_UNIT)) {
					log.warn("Something went wrong when generating the plans.");
				}
			} catch (InterruptedException e) {
			} catch (ExecutionException e) {
				log.error("Generating plans did not complete normally", e);
			} catch (TimeoutException e) {
				log.warn(
						"Generating plans did not finish yet. Unfiltered results are used in this run and planning will continue in the background until a new level is started.");
			}
			plans = currentLevel.tree.getCurrentNode().getChildrenPlans();
			success = shotSelector.generateAndEvaluateShotOptions();
		} else {
			plans = getTargets(loadedFrom);
			success = shotSelector.generateAndEvaluateShotOptions(plans);
		}
		String basename = "situation" + currentLevel.levelId + "-" + currentShot;
		showDemoPanel(basename);

		if (!success && loadedFrom == SceneLoaded.FROM_MEM) {
			currentLevel.ditchInitialScene();
			// Scene needs to be loaded again here so other parts can access Slingshot and
			// other stuff
			try {
				loadedFrom = prepareCurrentScene(img);
			} catch (SceneInitialisationException e) {
				return listener.sceneInitialisationFailed(img, e);
			}
			if (loadedFrom == SceneLoaded.NOT_LOADED)
				return false;
		}

		shotSelector.printAvailableTargets(); // print all available targets

		Shot shot = shotSelector.mostPromisingShot();

		listener.shotSelected(shot, shotSelector.getChosenTarget(), shotSelector.fallbackToDemoShot);

		if (shot == null) {
			currentLevel.currentScene = null;
			return false;
		}

		if (this.isFirstShot()) {
			log.debug("FIRST SHOT");
			log.debug("Gathering level features");

			this.currentLevel.updateFeatures(plans);
		}

		log.debug("Executing shot number " + currentShot);
		log.info(shot.prettyPrint());

		quickshot(shot);
		currentShot++;
		currentLevel.currentScene = null;
		return true;
	}

	/**
	 * Loads the current scene and stores it to {@link Level#currentScene}
	 *
	 * @param img Reusing screenshot from other analysis
	 * @return Where is this scene coming from. From memory or through Vision
	 */
	private SceneLoaded prepareCurrentScene(BufferedImage img) throws SceneInitialisationException {
		// first, acquire valid scene description
		AbstractScene s = currentLevel.tree.getCurrentNode().getScene();
		boolean sceneFromMemory = (s != null);

		if (!sceneFromMemory) {
			// if key objects are not detected, exception is thrown
			s = new Scene(img, currentLevel.getSlingshot(), currentLevel.getScalingFactor());
			if (!currentLevel.hasInitialScene() && isFirstShot())
				currentLevel.setInitialScene(s);
			currentLevel.currentScene = s;
			currentLevel.tree.getCurrentNode().setScene(s);
			currentLevel.tree.getCurrentNode().compareCurrentSituationWithPrevSituation();
		}

		// if bird count will change during shot sequence, adjust accordingly
		if (!currentLevel.numOfBirdsConfident) {
			currentLevel.numOfBirds = s.getBirds().size() + currentShot - 1; // each shot is a bird
			if (currentShot == currentLevel.numOfBirds) {
				// at this point we should have played till the last bird, so this should be
				// fixed
				currentLevel.numOfBirdsConfident = true;
			}
		}

		if (sceneFromMemory)
			return SceneLoaded.FROM_MEM;
		return SceneLoaded.NEW;
	}

	/**
	 * Generate all targets synchronously Evaluate reachability for all object and
	 * generate prolog file (or load from memory)
	 *
	 * @param loaded <br>
	 *               * {@code NEW} : return
	 *               {@link Strategy#planSynchronously(long)} <br>
	 *               * {@code FROM_MEM} : return {@link Level#initialPlans}
	 * @return List of Targets, may be null
	 */
	private List<Plan> getTargets(SceneLoaded loaded) {
		List<Plan> result;
		switch (loaded) {
			case FROM_MEM:
				result = currentLevel.initialPlans;
				break;

			case NEW:

				saveScreenshots();

				// write scene description for prolog planner and retrieve plans from
				// prolog (we may have stored the targets)
				try {
					Strategy strategy = strategyCallback.getStrategy(currentLevel, currentShot);
					List<Plan> plans = strategy.planSynchronously(Settings.STRATEGY_TIMEOUT);
					plans.sort(Comparator.comparingDouble(target -> -target.getConfidence())); // descending
					if (currentLevel.initialPlans == null) {
						currentLevel.initialPlans = plans;
					}
					result = plans;

				} catch (IllegalArgumentException e) {
					log.error("Error creating prolog files: " + e.getMessage(), e);
					result = null;
				}
				break;
			default:
				result = null;
				break;
		}
		return result;
	}

	/**
	 * Asynchronously generate plans for the current {@link Node}
	 *
	 * @param loaded only generate plans if {@link SceneLoaded#NEW}
	 *
	 * @return The future of the started job. get method returns true if the job
	 *         succeeded else false
	 */
	private Future<Boolean> generateTargets(SceneLoaded loaded) {
		if (loaded == SceneLoaded.NEW) {

			saveScreenshots();

			// create and run the strategy in background defined by strategyCallback
			return pool.submit(() -> {
				Strategy strategy = strategyCallback.getStrategy(currentLevel, currentShot);
				strategy.plan(new SimpleStrategyConsumer(currentLevel.tree.getCurrentNode()), Settings.STRATEGY_ASNYC_TIMEOUT);
				return true;
			});
		}
		// Return a simple future which returns the value true to indicate that no task
		// is running
		return new SimpleFuture<Boolean>(true);
	}

	private void saveScreenshots() {
		// TODO: This should maybe be done in debug module?
		log.debug("Sending screenshot request");
		pool.submit(() -> {
			String basename = "situation" + currentLevel.levelId + "-" + currentShot;
			SaveScreenshots.writeScreenshotWithIDs(currentLevel.currentScene, basename);
		});
		// no image written if disabled in VisualDebugger globally
	}

	/**
	 * Generate and show the demo panel for the current shot.
	 *
	 * @param basename Basename of the standalone .tex file (without .tex) to
	 *                 generate the jpg that will be displayed
	 */
	private void showDemoPanel(String basename) {
		// If in demo mode, compile/convert the generated tex file and update
		// VisualDebugger with img
		if (Settings.GAME_MODE == Settings.GameMode.DEMO && new File(basename + ".tex").exists()) {
			try {
				Process p = Runtime.getRuntime()
						.exec(new String[] { "pdflatex", "-shell-escape", "-halt-on-error", basename + ".tex" });
				p.waitFor();
				BufferedImage img = ImageIO.read(new File(basename + ".jpg"));
				DBG.setImage(img);
			} catch (IOException | InterruptedException e) {
				log.error(e);
			}
		}
	}

	/**
	 * Perform the shot, do some evaluation and wait till scene is stable
	 *
	 * @param shot This shot will be used to fire sling
	 * @throws ServerException
	 */
	private void quickshot(Shot shot) throws ServerException {
		byte result = Client.get().shootFast(shot);
		if (result == 0) {
			log.error("Shot was not valid");
			return;
		}
		try {
			Thread.sleep(SHOT_DELAY);
		} catch (InterruptedException ignored) {
		}
		int timesRetried = 0;
		while (timesRetried < 2) {
			try {
				listener.shotDelivered(shot);
				waitTillSceneIsStable();
				break;
			} catch (NullPointerException e) {
				log.error("NullPointerException occured while checking for finished shot. Retrying...");
				timesRetried++;
				if (timesRetried >= 2) {
					throw e;
				}
			}
		}
	}

	/**
	 * Continuously get screenshot and compare differences
	 *
	 * @throws ServerException if ServerConnection was lost or time is over
	 */
	private void waitTillSceneIsStable() throws ServerException {
		boolean isMoving = waitTillSceneIsStableNoListener(this.currentLevel);

		// new screen since old is blacked out, needed for realshape
		BufferedImage curScr = Client.get().doScreenShot();
		// TODO: move this to a better location
		DBG.saveToFileDirectly("quickshot-stable-" + DBG.incrementCounter(), curScr);

		// Scene seems stable, check if any pig is left -> level will quit soon
		boolean noPigsLeft = false;
		boolean noBirdsLeft = false;
		Vision v = new Vision(curScr);
		if (v.findPigsRealShape().size() == 0) { // killed all pigs, game takes time to notice
			log.info("Fastshot ended with no pigs left, waiting");
			noPigsLeft = true;
		} else if (isLastShot(v)) {
			log.info("Fastshot ended with pigs left but its the last shot, waiting");
			noBirdsLeft = true;
		}
		listener.shotDeliveredSceneStable(curScr, isMoving, noBirdsLeft, noPigsLeft);
	}

	/**
	 * Wait until the Scene is Stable without using {@link ShotExecutorCallback}
	 *
	 * @param level the currently played level
	 * @return true if the Scene is still moving
	 * @throws ServerException if ServerConnection was lost or time is over
	 */
	static public boolean waitTillSceneIsStableNoListener(Level level) throws ServerException {
		long shotTime = System.currentTimeMillis();
		long now = shotTime;
		long isOver = now + 16000; // wait max 16s
		boolean isMoving = true;
		BufferedImage curScr = VisionHelper.screenshotWithoutMovingParts(Client.get().doScreenShot());
		BufferedImage prevScr;

		int countZeroChangeFrames = 0;

		while (now < isOver && isMoving && Client.get().getGameState() == GameState.PLAYING) {	
			// single loop body takes ~100ms to execute, so no need to further delay using
			// sleep()
			prevScr = curScr;
			BufferedImage screenshot = Client.get().doScreenShot();
			curScr = VisionHelper.screenshotWithoutMovingParts(screenshot);
			updateScoreIfNecessary(screenshot, level);

			int dif = ImageUtil.pixelDifference(prevScr, curScr);
			now = System.currentTimeMillis();
			if (dif > 100) {
				countZeroChangeFrames = 0; // not stable yet
			} else {
				++countZeroChangeFrames;
				if (countZeroChangeFrames >= 2)
					isMoving = false;
			}

			log.debug(String.format("now: %1.2fs, pixel difference: %d", (now - shotTime) / 1000.0, dif));
		}
		// birdsXMax = 0;
		// birdsWidthMax = 0;
		log.info(String.format("waited: %1.2fs, still moving: %b", (now - shotTime + SHOT_DELAY) / 1000.0, isMoving));
		return isMoving;
	}

	@Override
	public void shutdown() {
		pool.shutdown();
		try {
			// Wait a while for existing tasks to terminate
			if (!pool.awaitTermination(1, TimeUnit.SECONDS)) {
				pool.shutdownNow(); // Cancel currently executing tasks
				// Wait a while for tasks to respond to being cancelled
				if (!pool.awaitTermination(1, TimeUnit.SECONDS))
					log.error("Pool did not terminate");
			}
		} catch (InterruptedException ie) {
			// (Re-)Cancel if current thread also interrupted
			pool.shutdownNow();
			// Preserve interrupt status
			Thread.currentThread().interrupt();
		}
	}

	/**
	 * Update a level's internal score based on image recognition from the screenshot.
	 * @param screenshot a screenshot to be analyzed
	 * @param level the level to be updated
	 */
	private static void updateScoreIfNecessary(BufferedImage screenshot, Level level) {
		GameStateExtractor extractor = new GameStateExtractor();
		int newScore = extractor.getScoreInGame(screenshot);

		if (newScore > level.currentScore) {
			level.currentScore = newScore;
		}
	}
}
