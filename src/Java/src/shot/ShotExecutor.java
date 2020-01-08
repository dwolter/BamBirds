package shot;

import ab.demo.other.Shot;
import ab.vision.Vision;
import ab.vision.real.shape.Poly;
import features.Scene;
import features.SceneInitialisationException;
import features.VisualDebugger;
import helper.CustomLogger;
import meta.ActionRobot;
import meta.Level;
import meta.ShotSelection;
import planner.Target;
import planner.Node;

import java.awt.image.BufferedImage;
import java.util.Comparator;
import java.util.List;

import static helper.Constants.DEBUG_ENABLED;

public class ShotExecutor {
	public enum SceneLoaded { NOT_LOADED, NEW, FROM_MEM;}
	public interface PrologCallback {
		List<Target> getTargets(Level level, String basename) throws IllegalArgumentException;
	}
	public interface ShotExecutorCallback {
		boolean sceneInitialisationFailed(BufferedImage screenshot, SceneInitialisationException exception);
		void shotSelected(Shot proposedShot, Target target, boolean demoShot);
		void shotDelivered(Shot shot);
		void shotDeliveredSceneStable(BufferedImage stableScreen, boolean stillMoving, boolean noBirds, boolean noPigs);
	}

	// Constant variables
	static private long SHOT_DELAY = 5000; // min time to wait, takes ~5s for birds to fly the longest parabola
	static private VisualDebugger DBG = new VisualDebugger("ShotExecutor");
	static{ DBG.enableDebug(false, false); }

	// instance variables
	private Level currentLevel;
	private PrologCallback pCallback;
	private ShotExecutorCallback listener;
	public int currentShot = 1;

	public ShotExecutor(Level level, PrologCallback callback, ShotExecutorCallback listener) {
		this.currentLevel = level;
		this.pCallback = callback;
		this.listener = listener;
	}

	public boolean isLastShot() { return (currentShot == currentLevel.numOfBirds); }
	public boolean isFirstShot() { return (currentShot == 1); }

	/**
	 * Do everything. From gathering the current scene, to Prolog planning,
	 * Shot selection, Shot execution and after-shot evaluation
	 * @param img Reusing screenshot from other analysis
	 * @return Returns true if shot was delivered successfully
	 */
	public boolean run(BufferedImage img) {
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
		boolean success = shotSelector.generateAndEvaluateShotOptions(plans, currentLevel.currentScene.getReachableTargets());
		if (DEBUG_ENABLED)
			shotSelector.printAvailableTarges(); // print all available targets

		Shot shot = shotSelector.mostPromisingShot();
		if (!success && loadedFrom == SceneLoaded.FROM_MEM) {
			currentLevel.ditchInitialScene();
		}

		listener.shotSelected(shot, shotSelector.getChosenTarget(), shotSelector.fallbackToDemoShot);

		if (shot == null) {
			currentLevel.currentScene = null;
			return false;
		}

		if(this.isFirstShot()) {
			CustomLogger.info("[ShotExecutor] FIRST SHOT");
			CustomLogger.info("[ShotExecutor] Gathering level features");

			// amount of pigs
			this.currentLevel.featureMap.put("num_pigs", this.currentLevel.currentScene.getPigs().size());
			// amount of birds
			this.currentLevel.featureMap.put("num_birds", this.currentLevel.numOfBirds);
			// amount of destroyable objects
			this.currentLevel.featureMap.put("num_destroyable_objects", this.currentLevel.currentScene.getPigs().size() + this.currentLevel.currentScene.getHills().size()
					+ this.currentLevel.currentScene.getTnts().size() + this.currentLevel.currentScene.getBlocks().size()); // pigs + hills + tns + blocks
			// amount of generated shots
			this.currentLevel.featureMap.put("num_generated_shots", this.currentLevel.currentScene.getCountSavedShots());
			// amount level has been played so far
			this.currentLevel.featureMap.put("num_times_played", this.currentLevel.numberOfTimesPlayed);
			// amount of generated plans
			this.currentLevel.featureMap.put("num_strategies", plans.size());

			// amount of line segments of polygons of hills
			int total_line_segments = this.currentLevel.currentScene.getHills().stream().mapToInt(h ->
				((Poly) h).polygon.npoints).sum(); // hills are for sure polygons so casting is safe

			this.currentLevel.featureMap.put("num_line_segments_hills", total_line_segments);

			// list of strings of strategies for all currently AVAILABLE targets
			// we only care about the newest strategies, so forget the old ones
			this.currentLevel.strategyTags.clear();
			for(Node node : shotSelector.getAvailableTargets()) {
				if(node.target != null) {
					this.currentLevel.strategyTags.add(node.target.getDebugInfo());
				}
			}

			// add here weighted sum of strategies, so that we don't have to calculate it later in the models
			int sum = this.currentLevel.calculateStrategiesWeightedSum();
			this.currentLevel.featureMap.put("numerical_strategies", sum);
		}

		CustomLogger.info("[ShotExecutor] Executing shot number " + currentShot);
		quickshot(shot);
		currentShot++;
		currentLevel.currentScene = null;
		return true;
	}

	/**
	 * Loads the current scene and stores it to {@link Level#currentScene}
	 * @param img Reusing screenshot from other analysis
	 * @return Where is this scene coming from. From memory or through Vision
	 */
	private SceneLoaded prepareCurrentScene(BufferedImage img) throws SceneInitialisationException {
		// first, acquire valid scene description
		Scene s = currentLevel.currentScene;
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
				// at this point we should have played till the last bird, so this should be fixed
				currentLevel.numOfBirdsConfident = true;
			}
		}

		if (sceneFromMemory)
			return SceneLoaded.FROM_MEM;
		return SceneLoaded.NEW;
	}

	/**
	 * Evaluate reachability for all object and generate prolog file (or load from memory)
	 * @param loaded <br> * {@code NEW} :      return {@link knowledge.Knowledge#buildModel(Level, String)}
	 *               <br> * {@code FROM_MEM} : return {@link Level#initialTargets}
	 * @return List of Targets, may be null
	 */
	private List<Target> getPrologPlans(SceneLoaded loaded) {
		switch (loaded) {
		case FROM_MEM:
			return currentLevel.initialTargets;

		case NEW:
			// this will create savedShots list for currentScene.getReachableTargets()
			currentLevel.calculateReachabilityForScene(currentLevel.currentScene);

			// second, write scene description for prolog planner and retrieve plans from prolog (we may have stored the targets)
			String basename = "situation" + currentLevel.levelId + "-" + currentShot;

			currentLevel.currentScene.writeScreenshotWithIDs(basename); // no image written if disabled in VisualDebugger globally
			try {
				List<Target> targets = pCallback.getTargets(currentLevel, basename);
				targets.sort(Comparator.comparingDouble(target -> -target.getConfidence())); // descending
				if (currentLevel.initialTargets == null) {
					currentLevel.initialTargets = targets;
				}
				return targets;

			} catch (IllegalArgumentException e) {
				CustomLogger.severe("[ShotExecutor] Error creating prolog files: " + e.getMessage());
				return null;
			}
		}
		return null;
	}

	/**
	 * Perform the shot, do some evaluation and wait till scene is stable
	 * @param shot This shot will be used to fire sling
	 */
	private void quickshot(Shot shot) {
		ActionRobot.get().shootFast(
				shot.getX(), shot.getY(), shot.getDx(), shot.getDy(), 0, (int)shot.getT_tap(), false);
		try {
			Thread.sleep(SHOT_DELAY);
		} catch (InterruptedException ignored) {}
		int timesRetried = 0;
		while (timesRetried<2){
			try {
				listener.shotDelivered(shot);
				waitTillSceneIsStable();
				break;
			} catch (NullPointerException e){
				CustomLogger.severe("NullPointerException occured while checking for finished shot. Retrying...");
				timesRetried++;
				if(timesRetried>=2){
					throw e;
				}
			}
		}
	}

	/**
	 * Continuously get screenshot and compare differences
	 */
	private void waitTillSceneIsStable() {
		long shotTime         = System.currentTimeMillis();
		long now              = shotTime;
		long isOver           = now + 16000 - SHOT_DELAY; // wait max 16s
		boolean isMoving      = true;
		BufferedImage curScr  = currentLevel.screenshotWithoutMovingParts();
		BufferedImage prevScr;

		int countZeroChangeFrames = 0;
		while (now < isOver && isMoving) {
			// single loop body takes ~100ms to execute, so no need to further delay using sleep()
			prevScr  = curScr;
			curScr   = currentLevel.screenshotWithoutMovingParts();
			int dif  = ActionRobot.get().pixelDifference(prevScr, curScr);
			now      = System.currentTimeMillis();
			if (dif > 5) {
				countZeroChangeFrames = 0; // not stable yet
			} else {
				++countZeroChangeFrames;
				if (countZeroChangeFrames >= 3)
					isMoving = false;
			}
			//CustomLogger.info(String.format("[ShotExecutor] now: %1.2fs, pixel difference: %d", (now-shotTime)/1000.0, dif));
		}

		curScr = ActionRobot.get().doScreenShot(); // new screen since old is blacked out, needed for real shape
		DBG.saveToFileDirectly("quickshot-stable-" + DBG.incrementCounter(), curScr);
		CustomLogger.info(String.format("[ShotExecutor] waited after shooting: %1.2fs, still moving: %b",
				(now - shotTime + SHOT_DELAY) / 1000.0, isMoving));


		// Scene seems stable, check if any pig is left -> level will quit soon
		boolean noPigsLeft = false;
		boolean noBirdsLeft = false;
		Vision v = new Vision(curScr);
		if (v.findPigsRealShape().size() == 0) { // killed all pigs, game takes time to notice
			CustomLogger.info("[ShotExecutor] Fastshot ended with no pigs left, waiting");
			noPigsLeft = true;
		} else if (isLastShot()) {
			CustomLogger.info("[ShotExecutor] Fastshot ended with pigs left but its the last shot, waiting");
			noBirdsLeft = true;
		}
		listener.shotDeliveredSceneStable(curScr, isMoving, noBirdsLeft, noPigsLeft);
	}
}
