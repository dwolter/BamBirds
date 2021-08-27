package de.uniba.sme.bambirds.common.objects;

import de.uniba.sme.bambirds.common.objects.ab.Slingshot;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import de.uniba.sme.bambirds.common.utils.FileUtil;
import de.uniba.sme.bambirds.common.utils.Settings;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Level {
	private static final Logger log = LogManager.getLogger(Level.class);
	public enum State {
		OPEN, WON, LOST, RESIGNED
	}

	public int levelId;
	private int maximalPointsWithoutPigs = 0;
	private int _bestScore = 0;
	private List<Integer> scores = new ArrayList<>();

	/** The (pending) current score at a point of time in the level */
	public int currentScore = 0;
	public int numberOfTimesPlayed = 0;
	public int numOfBirds = 0;
	public boolean numOfBirdsConfident = false;
	private Slingshot slingshot = null;
	private double scalingFactor = 1.005;

	private boolean safetyMeasuresEnabled = false;
	private boolean isDangerous = false;
	public int numFailedShots = 0;

	private State _state = State.OPEN;
	public List<Plan> initialPlans = null;
	transient public List<Triplet<Shot, Plan, Integer>> executedShots = new ArrayList<>();

	private AbstractScene initialScene = null;
	transient public AbstractScene currentScene = null;
	public DecisionTree tree = new DecisionTree();

	/** Contains a list of features that are interesting for our model **/

	public LinkedHashMap<String, Integer> featureMap = new LinkedHashMap<String, Integer>(10);

	/** Contains a list of strategies chosen in the first shot **/
	public List<String> strategyTags = new ArrayList<String>();

	/** The reward of a level is the achieved score if successfully completed */
	private int reward = 0;

	/**
	 * The cost of a level is the time in seconds it took to finish it (successfully
	 * or not), default 60
	 */
	private long costs = 60;

	private long startTimestamp = 0;

	private long currentDeltaTimestamp = 0;

	public Level(int levelid) {
		this.levelId = levelid;
	}

	public Slingshot getSlingshot() {
		return slingshot;
	}

	public double getScalingFactor() {
		return scalingFactor;
	}

	public boolean hasInitialScene() {
		return (initialScene != null);
	}

	public int getBestScore() {
		return _bestScore;
	}

	public List<Integer> getScores() {
		return Collections.unmodifiableList(scores);
	}

	public int getEstimatedMaximalPoints() {
		return this.maximalPointsWithoutPigs + (numOfBirds - 1) * 10000;
	}

	public void setInitialScene(AbstractScene s) {
		this.initialScene = s;
		this.maximalPointsWithoutPigs = 0;
		if (s != null) {
			this.maximalPointsWithoutPigs = s.estimateMaximalPointsWithoutBirds();
			setSlingshotAndScaling(s.slingshot, s.scalingFactor);
		}
	}

	public void ditchInitialScene() {
		setInitialScene(null);
		initialPlans = null;
		numOfBirds = 0;
		numOfBirdsConfident = false;
		slingshot = null;
		scalingFactor = 1.005;
		// TODO: reset limited time update slingshot counter once implemented
		tree = new DecisionTree();
		executedShots = new ArrayList<>();
	}

	public void finishLevel(GameState state, int score) {
		numberOfTimesPlayed++;
		setLevelState(state);

		// First put in the old best Score for the first round
		this.featureMap.put("max_score", this._bestScore);
		this.scores.add(score);

		if (state == GameState.WON) {
			this.currentScore = score;

			if (score > _bestScore) {
				_bestScore = score;
			}

			this.reward = score;
		}

		this.featureMap.put("num_score", this.currentScore);

		// levelId for DEBUGGING purposes
		this.featureMap.put("levelid", this.levelId);

		tree.finishGame(state);

		// get the time the level playthrough took
		final long finishTimestamp = System.currentTimeMillis();
		final long delta = (finishTimestamp - this.getStartTimestamp());

		// set timestamp in level here so that deviation from actual remaining time is
		// smaller
		this.setCurrentDeltaTimestamp(delta);

		// update costs of level
		this.setCosts(delta / 1000);

		log.debug("Level reward: " + this.getReward() + ", Costs: " + this.getCosts() + " seconds");

		if (Settings.EXPORT_LEVEL_STATS) {
			log.debug("Writing feature vector to csv file...");
			String featureVectorString = this.featureMap.entrySet().stream().map(Map.Entry::toString).map(String::valueOf)
					.collect(Collectors.joining(","));

			log.debug("featureVector = " + String.format("[%s]", featureVectorString));
			String strategyTagsString = this.strategyTags.stream().collect(Collectors.joining(","));
			log.debug("strategyTags = " + String.format("[%s]", strategyTagsString));

			this.writeCsvFile("featureList.csv", state);
		}

		// New best score needs to be put in for the LevelSelection
		this.featureMap.put("max_score", this._bestScore);

		resetToUnloadedState();
	}

	public void setLevelState(GameState state) {
		switch (state) {
		case WON:
		case LOST:
			// once won keep the level in that state no matter if we fail later
			if (_state != State.WON) {
				if (state == GameState.WON)
					_state = State.WON;
				else
					_state = State.LOST;
			}
			break;
		default:
			log.info("Recieved unexpected LevelState");
		}
	}

	public final State getLevelState() {
		return _state;
	}

	/** TODO: Use seperate setters */
	public void setSlingshotAndScaling(Slingshot sling, double scalingFactor) {
		boolean updated = false;
		if (sling != null) {
			this.slingshot = sling;
			updated = true;
		}
		if (scalingFactor > 0.1 && this.scalingFactor != scalingFactor) {
			this.scalingFactor = scalingFactor;
			updated = true;
		}
		if (updated) {
			// TODO: update only limited times, will prevent decision tree issues
		}
	}

	public void wasShotIneffective() {

	}

	public void resetToUnloadedState() {
		this.currentScene = this.initialScene;
		this.executedShots = new ArrayList<>();
		this.tree.resetToRootNode();
		this.numFailedShots = 0;
	}

	public void addExecutedShot(Shot executedShot, Plan plan, int damagePoints) {
		executedShots.add(new Triplet<>(executedShot, plan, damagePoints));
		log.debug("Level " + levelId + ": shot " + executedShots.size() + " added with " + damagePoints
				+ " damage points");
	}

	public void writeCsvFile(String filename, GameState gameState) {
		File f = null;

		try {
			f = new File(filename);
			if (!f.exists()) {
				f.createNewFile();
			}
		} catch (NullPointerException e) {
			log.error("Could not open csv file because pathname is null!");
			return;
		} catch (IOException e) {
			log.error("Could not create new File \"" + filename + "\" because " + e.getMessage());
			return;
		}

		StringBuilder builder = new StringBuilder();

		if (f.isFile() && f.length() == 0) {
			// write column list
			// String columnNameList =
			// "num_pigs,num_birds,num_destroyable_objects,num_generated_shots,num_times_played,num_strategies,num_line_segments_hills,num_score,max_score,levelid,list_strategies,game_won";
			String columnNameList = String.join(",",
					this.featureMap.entrySet().stream().map(Map.Entry::getKey).map(String::valueOf).collect(Collectors.toList()))
					+ ",list_strategies,game_won";
			builder.append(columnNameList + "\n");
		}

		// Now write content to csv file
		builder.append(String.join(",", this.featureMap.entrySet().stream().map(Map.Entry::getValue).map(String::valueOf)
				.collect(Collectors.toList())));

		builder.append("," + String.join(":", this.strategyTags));

		builder.append("," + gameState + "\n");

		FileUtil.write(filename, builder.toString(), true);
	}

	public int calculateStrategiesWeightedSum() {
		HashMap<String, Integer> strategyOccurrence = new HashMap<String, Integer>(Settings.STRATEGY_WEIGHTS.size());
		Set<String> strategySet;
		int strategiesWeightedSum = 0;

		// get the occurrence of each strategy in the strategy list
		for (String strategy : this.strategyTags) {
			if (Settings.STRATEGY_WEIGHTS.get(strategy) != null && !strategyOccurrence.containsKey(strategy)) {
				int strategyCount = Collections.frequency(this.strategyTags, strategy);
				strategyOccurrence.put(strategy, strategyCount);
			}
		}

		// convert into set, so that we don't weight the same strategies multiply times
		// later
		strategySet = new HashSet<String>(this.strategyTags);

		// calculate weighted sum of strategies
		for (String strategy : strategySet) {
			Integer weight = Settings.STRATEGY_WEIGHTS.get(strategy);
			if (weight != null) {
				strategiesWeightedSum += strategyOccurrence.get(strategy) * weight;
			}
		}

		return strategiesWeightedSum;
	}

	public void updateStartTimestamp() {
		this.startTimestamp = System.currentTimeMillis();
	}

	public long getCurrentDeltaTimestamp() {
		return currentDeltaTimestamp;
	}

	public int getReward() {
		return this.reward;
	}

	public long getCosts() {
		return this.costs;
	}

	public void setCosts(long costs) {
		this.costs = costs;
	}

	@Override
	public String toString() {
		return "Level: " + levelId + " estimatedMaxPoints: " + getEstimatedMaximalPoints() + " bestScore: " + _bestScore
				+ " numberOfTimesPlayed: " + numberOfTimesPlayed;
	}

	public void setCurrentDeltaTimestamp(long currentDeltaTimestamp) {
		this.currentDeltaTimestamp = currentDeltaTimestamp;
	}

	public long getStartTimestamp() {
		return this.startTimestamp;
	}

	public boolean safetyMeasuresEnabled() {
		return safetyMeasuresEnabled;
	}

	public boolean isDangerous() {
		return isDangerous;
	}

	public void setDangerous(boolean isDangerous) {
		if (this.isDangerous && _state != State.WON && numberOfTimesPlayed > 1) {
			_state = State.RESIGNED;
			return;
		}
		safetyMeasuresEnabled = true;
		this.isDangerous = isDangerous;
		this.ditchInitialScene();
	}

	/**
	 * Saves or updates the current level features and targets
	 * @param plans generated prolog plans
	 */
	public void updateFeatures(List<Plan> plans) {
		// amount of pigs
		this.featureMap.put("num_pigs", this.currentScene.getPigs().size());
		// amount of birds
		this.featureMap.put("num_birds", this.numOfBirds);
		// amount of destroyable objects
		this.featureMap.put("num_destroyable_objects", this.currentScene.getPigs().size() + this.currentScene.getHills().size()
				+ this.currentScene.getTnts().size() + this.currentScene.getBlocks().size()); // pigs + hills + tnts + blocks
		// amount of generated shots
		//TODO: remove
		this.featureMap.put("num_generated_shots", 0);
		// amount level has been played so far
		this.featureMap.put("num_times_played", this.numberOfTimesPlayed);
		// amount of generated plans
		this.featureMap.put("num_strategies", plans.size());

		// amount of line segments of polygons of hills
		int total_line_segments = this.currentScene.getHills().stream().mapToInt(h ->
			((Poly) h).polygon.npoints).sum(); // hills are for sure polygons so casting is safe

		this.featureMap.put("num_line_segments_hills", total_line_segments);

		// list of strings of strategies for all currently AVAILABLE targets
		// we only care about the newest strategies, so forget the old ones
		this.strategyTags.clear();
		for(Plan plan : plans) {
			if(plan != null) {
				this.strategyTags.add(plan.getStrategy());
			}
		}

		// add here weighted sum of strategies, so that we don't have to calculate it later in the models
		int sum = calculateStrategiesWeightedSum();
		this.featureMap.put("numerical_strategies", sum);
	}
}
