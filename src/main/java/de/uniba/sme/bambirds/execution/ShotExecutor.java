package de.uniba.sme.bambirds.execution;

import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.vision.Vision;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;
import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.BamBirdModule;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.AbstractScene;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.vision.SaveScreenshots;
import de.uniba.sme.bambirds.vision.Scene;
import de.uniba.sme.bambirds.vision.VisionHelper;
import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.objects.Target;
import de.uniba.sme.bambirds.common.objects.TargetPoint;
import de.uniba.sme.bambirds.planner.shot.ShotPlanner;
import de.uniba.sme.bambirds.planner.shot.ShotSelection;

import java.awt.image.BufferedImage;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ShotExecutor extends BamBirdModule {
	private static final Logger log = LogManager.getLogger(ShotExecutor.class);

	static private VisualDebugger DBG = new VisualDebugger("ShotExecutor");
	{ DBG.enableDebug(false, true); }

	public enum SceneLoaded {
		NOT_LOADED, NEW, FROM_MEM;
	}

	public interface PrologCallback {
		List<Target> getTargets(Level level, String basename) throws IllegalArgumentException;
	}

	public interface ShotExecutorCallback {
		boolean sceneInitialisationFailed(BufferedImage screenshot, SceneInitialisationException exception)
				throws ServerException;

		void shotSelected(Shot proposedShot, Target target, boolean demoShot);

		void shotDelivered(Shot shot) throws ServerException;

		void shotDeliveredSceneStable(BufferedImage stableScreen, boolean stillMoving, boolean noBirds, boolean noPigs)
				throws ServerException;
	}

	// Constant variables
	static private long SHOT_DELAY = 5000; // min time to wait, takes ~5s for birds to fly the longest parabola

	// instance variables
	private Level currentLevel;
	private PrologCallback pCallback;
	private ShotExecutorCallback listener;
	private ExecutorService pool;
	public int currentShot = 1;

	public ShotExecutor(Level level, PrologCallback callback, ShotExecutorCallback listener) {
		this.currentLevel = level;
		this.pCallback = callback;
		this.listener = listener;
		this.pool = Executors.newCachedThreadPool();
	}

	public boolean isLastShot() {
		return (currentShot == currentLevel.numOfBirds);
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
		SceneLoaded loadedFrom = SceneLoaded.NOT_LOADED;
		try {
			loadedFrom = prepareCurrentScene(img);
		} catch (SceneInitialisationException e) {
			return listener.sceneInitialisationFailed(img, e);
		}
		if (loadedFrom == SceneLoaded.NOT_LOADED)
			return false;

		List<Target> plans = getPrologPlans(loadedFrom);

		ShotSelection shotSelector = new ShotSelection(currentLevel, currentShot - 1);
		boolean success = shotSelector.generateAndEvaluateShotOptions(plans, currentLevel.currentScene.getPossibleShots());

		if (!success && loadedFrom == SceneLoaded.FROM_MEM) {
			currentLevel.ditchInitialScene();
		}

		shotSelector.printAvailableTargets(); // print all available targets

		Shot shot = shotSelector.mostPromisingShot();

		listener.shotSelected(shot, shotSelector.getChosenTarget(), shotSelector.fallbackToDemoShot);

		if (shot == null) {
			currentLevel.currentScene = null;
			return false;
		}

		if (this.isFirstShot()) {
			log.info("FIRST SHOT");
			log.info("Gathering level features");

			this.currentLevel.updateFeatures(plans);
		}

		log.info("Executing shot number " + currentShot);
		log.info(shot.toString());

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
		AbstractScene s = currentLevel.currentScene;
		boolean sceneFromMemory = (s != null);

		if (s == null) {
			// if key objects are not detected, exception is thrown
			s = new Scene(img, currentLevel.getSlingshot(), currentLevel.getScalingFactor());
			if (!currentLevel.hasInitialScene() && currentShot == 1)
				currentLevel.setInitialScene(s);
			currentLevel.currentScene = s;
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
	 * Evaluate reachability for all object and generate prolog file (or load from
	 * memory)
	 * 
	 * @param loaded <br>
	 *               * {@code NEW} : return
	 *               {@link knowledge.Knowledge#buildModel(Level, String)} <br>
	 *               * {@code FROM_MEM} : return {@link Level#initialTargets}
	 * @return List of Targets, may be null
	 */
	private List<Target> getPrologPlans(SceneLoaded loaded) {
		switch (loaded) {
			case FROM_MEM:
				return currentLevel.initialTargets;

			case NEW:

				// first calculate possible targets (!! NO Calculation of actual reachability)
				List<TargetPoint> targetPoints = currentLevel.currentScene.getTargetPoints();
				ShotPlanner planner = new ShotPlanner(currentLevel);
				for (TargetPoint targetPoint : targetPoints) {
					currentLevel.currentScene.addPossibleShots(planner.savedShotsForTarget(targetPoint));
				}

				// second, write scene description for prolog planner and retrieve plans from
				// prolog (we may have stored the targets)
				String basename = "situation" + currentLevel.levelId + "-" + currentShot;

				// TODO: This should maybe be done in debug module?
				log.debug("Sending screenshot request");
				pool.submit(() -> {
					SaveScreenshots.writeScreenshotWithIDs(currentLevel.currentScene, basename);
				});
				// no image written if disabled in VisualDebugger globally

				try {
					List<Target> targets = pCallback.getTargets(currentLevel, basename);
					targets.sort(Comparator.comparingDouble(target -> -target.getConfidence())); // descending
					if (currentLevel.initialTargets == null) {
						currentLevel.initialTargets = targets;
					}
					return targets;

				} catch (IllegalArgumentException e) {
					log.error("Error creating prolog files: " + e.getMessage(), e);
					return null;
				}
			default:
				break;
		}
		return null;
	}

	/**
	 * Perform the shot, do some evaluation and wait till scene is stable
	 * 
	 * @param shot This shot will be used to fire sling
	 * @throws ServerException
	 */
	private void quickshot(Shot shot) throws ServerException {
		Client.get().shootFast(shot);
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

		boolean isMoving = waitTillSceneIsStableNoListener();

		BufferedImage curScr = Client.get().doScreenShot(); // new screen since old is blacked out, needed for real shape
		// TODO: move this to a better location
		DBG.saveToFileDirectly("quickshot-stable-" + DBG.incrementCounter(), curScr);

		// Scene seems stable, check if any pig is left -> level will quit soon
		boolean noPigsLeft = false;
		boolean noBirdsLeft = false;
		Vision v = new Vision(curScr);
		if (v.findPigsRealShape().size() == 0) { // killed all pigs, game takes time to notice
			log.info("Fastshot ended with no pigs left, waiting");
			noPigsLeft = true;
		} else if (isLastShot()) {
			log.info("Fastshot ended with pigs left but its the last shot, waiting");
			noBirdsLeft = true;
		}
		listener.shotDeliveredSceneStable(curScr, isMoving, noBirdsLeft, noPigsLeft);
	}

	/**
	 * Wait until the Scene is Stable without using {@link ShotExecutorCallback}
	 * 
	 * @return true if the Scene is still moving
	 * @throws ServerException if ServerConnection was lost or time is over
	 */
	static public boolean waitTillSceneIsStableNoListener() throws ServerException {
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
			curScr = VisionHelper.screenshotWithoutMovingParts(Client.get().doScreenShot());
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
			if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
				pool.shutdownNow(); // Cancel currently executing tasks
				// Wait a while for tasks to respond to being cancelled
				if (!pool.awaitTermination(60, TimeUnit.SECONDS))
					log.error("Pool did not terminate");
			}
		} catch (InterruptedException ie) {
			// (Re-)Cancel if current thread also interrupted
			pool.shutdownNow();
			// Preserve interrupt status
			Thread.currentThread().interrupt();
		}
	}
}
