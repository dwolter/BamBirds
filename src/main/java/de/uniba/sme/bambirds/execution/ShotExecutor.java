package de.uniba.sme.bambirds.execution;

import de.uniba.sme.bambirds.common.database.Level;
import de.uniba.sme.bambirds.common.database.Node;
import de.uniba.sme.bambirds.common.objects.ExecutedNode;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.ImageUtil;
import de.uniba.sme.bambirds.common.utils.SimpleFuture;
import de.uniba.sme.bambirds.common.utils.VisualDebugger;
import de.uniba.sme.bambirds.common.BamBirdModule;
import de.uniba.sme.bambirds.common.Strategy;
import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.exceptions.ShotRejectedByServerException;
import de.uniba.sme.bambirds.common.exceptions.SceneInitialisationException.Reason;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.common.objects.Plan;
import de.uniba.sme.bambirds.common.objects.Timer;
import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.feedback.FeedbackManager;
import de.uniba.sme.bambirds.feedback.FeedbackScene;
import de.uniba.sme.bambirds.feedback.SceneCalculator;
import de.uniba.sme.bambirds.feedback.SequenceCalculator;
import de.uniba.sme.bambirds.feedback.ShotInformationController;
import de.uniba.sme.bambirds.planner.shot.EffectCalculator;
import de.uniba.sme.bambirds.planner.shot.MetaShotSelection;
import de.uniba.sme.bambirds.planner.shot.SceneDifference;
import de.uniba.sme.bambirds.planner.shot.ShotSelection;
import de.uniba.sme.bambirds.vision.GameStateExtractor;
import de.uniba.sme.bambirds.vision.SaveScreenshots;
import de.uniba.sme.bambirds.vision.Scene;
import de.uniba.sme.bambirds.vision.VisionHelper;
import de.uniba.sme.bambirds.vision.VisionMBR;
import de.uniba.sme.bambirds.vision.Vision;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.lang.ProcessBuilder.Redirect;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class ShotExecutor extends BamBirdModule {
	private static final Logger log = LogManager.getLogger(ShotExecutor.class);

	static private VisualDebugger DBG = new VisualDebugger("ShotExecutor");

	{
		DBG.enableDebug(false, false);
	}

	public enum SceneLoaded {
		NOT_LOADED, NEW, FROM_MEM;
	}

	public interface StrategyCallback {
		Strategy getStrategy(Level level, int currentShot) throws IllegalArgumentException;
	}

	public interface ShotExecutorCallback {
		boolean sceneInitialisationFailed(BufferedImage screenshot, SceneInitialisationException exception)
				throws ServerException;

		void nodeSelected(Node node, boolean demoShot);

		void shotDelivered(Shot shot) throws ServerException;

		void shotDeliveredSceneStable(BufferedImage stableScreen, boolean stillMoving, boolean noBirds, boolean noPigs)
				throws ServerException;
	}

	// Constant variables
	static private long SHOT_DELAY = 5000; // min time to wait, takes ~5s for birds to fly the longest parabola
	static private long SHOT_IMG_DELAY = 400; // min time while ongoing shot between two screenshots
	static private long MOVING_TIME = 2000; // time after shot in which objects are probably still moving
	static private long MAX_TIME_WAIT_TILL_STABLE = 21000 - SHOT_DELAY - MOVING_TIME; // max time after shot to wait till scene stable

	// instance variables
	private Level currentLevel;
	private StrategyCallback strategyCallback;
	private ShotExecutorCallback listener;
	private ExecutorService pool;
	public int currentShot = 1;
	private ShotInformationController shotInformation;

	private FeedbackManager feedbackManager;


	public ShotExecutor(Level level, StrategyCallback callback, ShotExecutorCallback listener, FeedbackManager manager, ShotInformationController shotInformation) {
		this.currentLevel = level;
		this.strategyCallback = callback;
		this.listener = listener;
		this.pool = Executors.newWorkStealingPool();
		this.feedbackManager = manager;
		this.shotInformation = shotInformation;
		shotInformation.setLevelInformation(currentLevel);
		DBG.enableDebug(Settings.GAME_MODE == Settings.GameMode.DEMO, Settings.VISUAL_DEBUG_ENABLED);
	}

	public boolean isLastShot(AbstractScene scene) {
		// If we had not more shots than the number of Birds it is not the last shot
		if (currentLevel.numOfBirds != currentShot)
			return false;
		// It is the last shot if there are no birds or the found bird is not in the slingshot
		List<ABObject> birds = scene.getBirds();
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
		MetaShotSelection shotSelector;
		List<Plan> plans;

		SceneLoaded loadedFrom = SceneLoaded.NOT_LOADED;
		try {
			// Very important since the level slingshot
			img = ensureFocusOnSling(img);
			loadedFrom = prepareCurrentScene(img);
		} catch (SceneInitialisationException e) {
			return listener.sceneInitialisationFailed(img, e);
		}
		if (loadedFrom == SceneLoaded.NOT_LOADED) {
			return false;
		}

		// FIXME: For some reason there is a NullPointerException thrown here in some circumstances.
		// Don't really know why and it is also not really reproducible as it never happens when debugging
		try {
			// If the bird count equals the count in the previous node it is safe to assume that there was no bird shot
			if (currentLevel.tree.getCurrentNode().parent != null &&
					currentLevel.tree.getCurrentNode().parent.getScene() != null &&
					currentLevel.currentScene.getBirds().size() == currentLevel.tree.getCurrentNode().parent.getScene().getBirds().size())
				currentLevel.numFailedShots++;
		} catch (NullPointerException ignored) {
		}

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
			} catch (InterruptedException ignored) {
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

		//((FeedbackShotSelection) shotSelector).exportTree(); // debug option //TODO: what shotselectiontype
		shotSelector.printAvailableTargets(); // print all available targets
		Node selectedNode = shotSelector.mostPromisingNode();

		if (Settings.FEEDBACK_ENABLED) {
			shotInformation.setRemovedShotsCounter(((ShotSelection) shotSelector).getRemovedShotCounter());
			shotInformation.setChosenTarget(shotSelector.getChosenTarget());
			shotInformation.setDemoShot(shotSelector.isFallbackToDemoShot());
			shotInformation.setSceneBeforeShot(currentLevel.currentScene);
		}

		listener.nodeSelected(selectedNode, shotSelector.isFallbackToDemoShot());

		if (selectedNode == null) {
			currentLevel.currentScene = null;
			return false;
		}

		if (this.isFirstShot()) {
			log.debug("FIRST SHOT");
			log.debug("Gathering level features");

			this.currentLevel.updateFeatures(plans);
		}

		log.debug("Executing shot number " + currentShot);
		log.info(selectedNode.getShot().prettyPrint());

		if (Settings.FEEDBACK_ENABLED) {
			this.shotInformation.setShot(selectedNode.getShot());
		}

		try {
			quickshot(selectedNode.getShot());
		} catch (ShotRejectedByServerException e) {
			// TODO Unsure how to best handle this case
			log.error(e);
		}

		currentShot++;
		currentLevel.currentScene = null;
		return true;
	}

	public BufferedImage ensureFocusOnSling(BufferedImage currentImage) throws ServerException, SceneInitialisationException {
		Slingshot levelSlingshot = currentLevel.getSlingshot();
		if (levelSlingshot != null) {
			// Ensure that focus is on the correct slingshot
			VisionMBR vision = new VisionMBR(ImageUtil.removeABUI(currentImage,0));
			Rectangle sling = vision.findSlingshotMBR();

			if (sling == null || sling.x < levelSlingshot.x) {
				log.debug("Level slingshot is more left, clicking");
				Client.get().clickInCenter();
				return Client.get().doScreenShot();
			} else {
				return currentImage;
			}
		} else {
			// Find the focus for the slingshot and move there

			Client.get().clickInCenter();
			BufferedImage secondImg = Client.get().doScreenShot();

			VisionMBR firstVision = new VisionMBR(ImageUtil.removeABUI(currentImage,0));
			Rectangle firstSling = firstVision.findSlingshotMBR();

			VisionMBR secondVision = new VisionMBR(ImageUtil.removeABUI(secondImg,0));
			Rectangle secondSling = secondVision.findSlingshotMBR();

			if (firstSling == null && secondSling == null) {
				throw new SceneInitialisationException(Reason.NO_SLING_FOUND);
			}

			if (firstSling == null || firstSling.x <= secondSling.x) {
				log.debug("Second screenshot is more left, can leave as is");
				return secondImg;
			} else {
				log.debug("First screenshot is more left, clicking");
				Client.get().clickInCenter();
				return currentImage;
			}
		}
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
			if (!currentLevel.hasInitialScene() && isFirstShot()) {
				currentLevel.setInitialScene(s);
			}
			currentLevel.currentScene = s;
			Node currentNode = currentLevel.tree.getCurrentNode();
			currentNode.setScene(s);
			currentNode.compareCurrentSituationWithPrevSituation();
		} else {
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

		if (sceneFromMemory) {
			return SceneLoaded.FROM_MEM;
		}
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

				saveScreenshots(null);

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
	 * @return The future of the started job. get method returns true if the job
	 * succeeded else false
	 */
	private Future<Boolean> generateTargets(SceneLoaded loaded) {
		if (loaded == SceneLoaded.NEW) {

			saveScreenshots(null);

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

	private void saveScreenshots(String fileName) {
		// TODO: This should maybe be done in debug module?
		log.debug("Sending screenshot request");
		pool.submit(() -> {
			String basename = "situation" + currentLevel.levelId + "-" + currentShot;
			if (fileName == null) {
				SaveScreenshots.writeScreenshotWithIDs(currentLevel.currentScene, basename);
			} else {
				SaveScreenshots.writeOneScreenshotWithIDs(currentLevel.currentScene, fileName);
			}
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
				ProcessBuilder builder = new ProcessBuilder("pdflatex", "-shell-escape", "-halt-on-error", basename + ".tex");
				// Redirect Latex to standard output, so we know something is happening
				builder.redirectOutput(Redirect.INHERIT);
				builder.redirectError(Redirect.INHERIT);
				Process p = builder.start();
				// Latex should not take that long, but just in case
				p.waitFor(1, TimeUnit.MINUTES);
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
	 * @throws ShotRejectedByServerException when the Shot was rejected
	 */
	private void quickshot(final Shot shot) throws ServerException, ShotRejectedByServerException {
		//TODO get first screenshot
		BufferedImage firstScreenshot = VisionHelper.screenshotWithoutMovingParts(Client.get().doScreenShot());

		boolean succeeded = Client.get().shootFast(shot);
		if (!succeeded) {
			throw new ShotRejectedByServerException("Shot was not valid");
		}
		/* sequences of objects over time while they are moving and scene is not stable. */
		waitTillSceneIsStable(firstScreenshot, shot);
	}

	private void waitTillSceneIsStable(BufferedImage firstScreenshot, final Shot shot) throws ServerException {
		try {
			ensureFocusOnSling(Client.get().doScreenShot());
		} catch (SceneInitialisationException ignored) { }
		List<Future<FeedbackScene>> feedbackFutures = new ArrayList<>();
		List<FeedbackScene> feedbackSceneList = new ArrayList<>();
		int sceneCounter = 0;
		/* Not necessary to compare whole image. Therefore searching a part of the image with all the objects and a little padding. */
		int[] cutoff = {-1, -1, -1, -1};
		if (Settings.FEEDBACK_ENABLED) {
			List<ABObject> objects = new ArrayList<>();
			objects.addAll(currentLevel.currentScene.getBlocks());
			objects.addAll(currentLevel.currentScene.getPigs());
			if (!objects.isEmpty()) {
				// Min X
				cutoff[0] = Math.max(0,(int) objects.stream().min(Comparator.comparing(ABObject::getCenterX)).get().getCenterX() - 80);
				// Max X
				cutoff[1] = Math.min(Settings.IMAGE_WIDTH,(int) objects.stream().max(Comparator.comparing(ABObject::getCenterX)).get().getCenterX() + 80);
				// Min Y
				cutoff[2] = Math.max(0,(int) objects.stream().min(Comparator.comparing(ABObject::getCenterY)).get().getCenterY() - 30);
				// Max Y
				cutoff[3] = currentLevel.currentScene.getGroundPlane();
			}
		}
		Timer flightTime = new Timer();
		BufferedImage origPrevScr = firstScreenshot;
		BufferedImage origCurScr = firstScreenshot;
		BufferedImage prevScr = ImageUtil.removeABUIAndClip(firstScreenshot, cutoff);
		BufferedImage curScr;
		boolean shotDelivered = false;

		int countZeroChangeFrames = 0;

		while (flightTime.getElapsedTime() < MAX_TIME_WAIT_TILL_STABLE 
			&& Client.get().getGameState() == GameState.PLAYING
			&& countZeroChangeFrames < 2) {
			// Send shot delivered once 
			if (!shotDelivered && flightTime.getElapsedTime() > SHOT_DELAY) {
				try {
					listener.shotDelivered(shot);
					shotDelivered = true;
				} catch (NullPointerException e) {
					log.error("Failed to send shot delivered to listener", e);
				}
			}
			
			/* TODO: Integrate advanced parsing of changed pixels. */
			try {
				try {
					origCurScr = Client.get().doScreenShot();
					curScr = ImageUtil.removeABUIAndClip(origCurScr, cutoff);
				} catch (ServerException e) {
					log.error("Failed to take image: " + e.getMessage());
					throw new InterruptedException();
				}

				int pixelDiff = ImageUtil.pixelDifference(prevScr, curScr, 0);
				// Calculate Feedback-Scenes only after changes in the scene
				if (pixelDiff > 100) {
					countZeroChangeFrames = 0;
					log.debug("Scene different by {} pixels", pixelDiff);
					origPrevScr = origCurScr;
					prevScr = curScr;
					if (Settings.FEEDBACK_ENABLED) {
						Future<FeedbackScene> sceneFuture = pool.submit(new SceneCalculator(origCurScr, curScr, cutoff, sceneCounter));
						feedbackFutures.add(sceneFuture);
						sceneCounter++;
					}
				} else if (shotDelivered) {
					// Increase zeroFrameChanges after the shot has been (supposedly) delivered
					// Should stop early if the shot has not been delivered or it failed
					log.debug("Scene equal");
					countZeroChangeFrames++;
				}
			} catch (InterruptedException ex) {
				log.error("Got interrupted while capturing ongoing shot.");
			}
		}
		if (Settings.FEEDBACK_ENABLED) {
			for (Future<FeedbackScene> future : feedbackFutures) {
				FeedbackScene sceneResult = null;
				try {
					sceneResult = future.get(700, TimeUnit.MILLISECONDS);
				} catch (InterruptedException | ExecutionException | TimeoutException exception) {
					log.error("Scene while ongoing shot was not captured.", exception);
				}
				if (sceneResult != null) {
					feedbackSceneList.add(sceneResult);
				}
			}
			if (feedbackSceneList.size() > 1) {
				/* at least two scenes necessary to compare */
				feedbackSceneList.sort(Comparator.comparing(scene -> scene.getId()));
				SequenceCalculator sequenceCalculator = new SequenceCalculator(feedbackSceneList);
				sequenceCalculator.calculate();
				shotInformation.setCalculator(sequenceCalculator);
			}
		}

		BufferedImage lastScrPlaying = GameStateExtractor.getGameState(origCurScr) == GameState.PLAYING ? origCurScr : origPrevScr;

		// Finalize things before reporting scene stable
		updateScoreIfNecessary(lastScrPlaying, currentLevel);
		// TODO: move this to a better location
		if (DBG.canOutputImage()) {
			DBG.saveToFileDirectly("quickshot-stable-" + currentLevel.levelId + "-" + currentShot + "-" + DBG.incrementCounter(), lastScrPlaying);
		}
		// Scene seems stable, check if any pig is left -> level will quit soon
		boolean noPigsLeft = false;
		boolean noBirdsLeft = false;
		Vision v = new Vision(lastScrPlaying);
		AbstractScene scene = new AbstractScene(lastScrPlaying) {
			public List<ABObject> getPigs() {
				return v.findPigsRealShape();
			};
			public List<ABObject> getBirds() {
				return v.findBirdsRealShape();
			};
			public List<ABObject> getBlocks() {
				// Blocks not necessary here
				return new ArrayList<>();
			}
		};
		// Analyze difference between shots and add executed node
		Node currentNode = currentLevel.tree.getCurrentNode();
		if (currentNode.parent != null) {
			SceneDifference difference = new SceneDifference(currentNode.getSceneBefore(),scene);
			ExecutedNode executedNode = EffectCalculator.analyzeEffects(difference, currentNode);
			currentLevel.tree.addExecutedNode(executedNode);
			log.debug("Added ExecutedNode {}", executedNode);
		}
		List<ABObject> remainingPigs = v.findPigsRealShape();
		if (remainingPigs.size() == 0) { // killed all pigs, game takes time to notice
			log.info("Fastshot ended with no pigs left");
			noPigsLeft = true;
		} else if (isLastShot(scene)) {
			log.info("Fastshot ended with pigs left but its the last shot");
			log.info("Saving remaining pigs to priorities");
			currentLevel.tree.addPriorityPigs(remainingPigs);
			noBirdsLeft = true;
		}

		listener.shotDeliveredSceneStable(lastScrPlaying, countZeroChangeFrames < 2, noBirdsLeft, noPigsLeft);
	}

	/**
	 * Wait until the Scene is Stable without using {@link ShotExecutorCallback}
	 *
	 * @param firstScreenshot Screenshot before starting the waiting
	 * @return true if the Scene is still moving
	 * @throws ServerException if ServerConnection was lost or time is over
	 */
	public static boolean waitTillSceneIsStableNoListener(BufferedImage firstScreenshot) throws ServerException {
		long shotTime = System.currentTimeMillis();
		long now = shotTime;
		long isOver = now + MAX_TIME_WAIT_TILL_STABLE;
		boolean isMoving = true;
		BufferedImage curScr;
		BufferedImage prevScr;
		if (firstScreenshot == null) {
			prevScr = VisionHelper.screenshotWithoutMovingParts(Client.get().doScreenShot());
		} else {
			prevScr = firstScreenshot;
		}
		BufferedImage changedPixels = new BufferedImage(prevScr.getWidth(), prevScr.getHeight(), prevScr.getType());
		String uuid = UUID.randomUUID().toString();
		int pixelThreshold = 0;
		Graphics2D g2d = (Graphics2D) changedPixels.getGraphics();
		// Clear the background with white
		g2d.setBackground(Color.WHITE);
		g2d.clearRect(0, 0, prevScr.getWidth(), prevScr.getHeight());
		g2d.dispose();

		//schwarze blöcke: regions of impact
		//TODO nicht gelöstes level: alle schüsse nicht geklappt:
		//sequenz von disjunkten schüssen: lösungssttrategie (kombination daraus)
		//TODO zeiptunkt: anfang u. ende, s. quickshot

		int countZeroChangeFrames = 0;

		while (now < isOver && isMoving && Client.get().getGameState() == GameState.PLAYING) {
			// single loop body takes ~100ms to execute, so no need to further delay using
			// sleep()
			BufferedImage screenshot = Client.get().doScreenShot();
			curScr = VisionHelper.screenshotWithoutMovingParts(screenshot);
			int diff = ImageUtil.pixelDifference(prevScr, curScr, changedPixels, pixelThreshold);
			now = System.currentTimeMillis();
			if (diff > 100) {
				countZeroChangeFrames = 0; // not stable yet
				//ImageUtil.pixelDifference(prevScr, curScr, changedPixels);
			} else {
				++countZeroChangeFrames;
				if (countZeroChangeFrames >= 2) {
					isMoving = false;
				}
			}
			log.debug(String.format("now: %1.2fs, pixel difference: %d", (now - shotTime) / 1000.0, diff));
			prevScr = curScr;
		}
		log.info(String.format("waited: %1.2fs, still moving: %b", (now - shotTime + SHOT_DELAY + MOVING_TIME) / 1000.0, isMoving));
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
				if (!pool.awaitTermination(1, TimeUnit.SECONDS)) {
					log.error("Pool did not terminate");
				}
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
	 *
	 * @param screenshot a screenshot to be analyzed
	 * @param level      the level to be updated
	 */
	private static void updateScoreIfNecessary(BufferedImage screenshot, Level level) throws ServerException {
		int newScore = 0;
		if (Settings.SERVER_TYPE == Settings.ServerType.SCIENCE_BIRDS) {
			newScore = Client.get().getCurrentLevelScoreInt();
		} else {
			GameStateExtractor.getScoreInGame(screenshot);
		}
		level.updateScore(newScore);
	}
}
