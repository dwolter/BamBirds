package meta;

import ab.demo.other.Shot;
import ab.vision.ABObject;
import ab.vision.ABType;
import ab.vision.GameStateExtractor;
import ab.vision.GameStateExtractor.GameState;
import database.Slingshot;
import features.Scene;
import helper.CustomLogger;
import level_selection.Prediction;
import planner.DecisionTree;
import planner.Target;
import shot.ShotPlanner;
import static helper.Constants.PERFORMANCE_MEASUREMENT_ENABLED;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
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

public class Level {
	public enum State {
		OPEN, WON, LOST, RESIGNED
	}

	public int levelId;
	private int maximalPointsWithoutPigs = 0;
	private int _bestScore = 0;
	/** The (pending) current score at a point of time in the level */
	public int currentScore = 0;
	public int numberOfTimesPlayed = 0;
	public int numOfBirds = 0;
	public boolean numOfBirdsConfident = false;
	private Slingshot slingshot = null;
  private double scalingFactor = 1.005;
  
  private boolean safetyMeasuresEnabled = false;
  private boolean isDangerous = false;

	private State _state = State.OPEN;
	public List<Target> initialTargets = null;
	transient public List<Triplet<Shot, Target, Integer>> executedShots = new ArrayList<>();

	private Scene initialScene = null;
	transient public Scene currentScene = null;
	public DecisionTree tree = new DecisionTree();

	/** Contains a list of features that are interesting for our model **/

	public LinkedHashMap<String, Integer> featureMap = new LinkedHashMap<String, Integer>(10);

	/** Contains a list of strategies chosen in the first shot **/
	public List<String> strategyTags = new ArrayList<String>();

	/** The reward of a level is the achieved score if successfully completed */
	private int reward = 0;

	/** The cost of a level is the time in seconds it took to finish it (successfully or not), default 60 */
	private long costs = 60;

	private long startTimestamp = 0;

	private long currentDeltaTimestamp = 0;

	public Level(int levelid) {
		this.levelId = levelid;
	}

	public Slingshot getSlingshot() { return slingshot; }
	public double getScalingFactor() { return scalingFactor; }
	public boolean hasInitialScene() { return (initialScene != null); }
	public int getBestScore() { return _bestScore; }
	public int getEstimatedMaximalPoints() { return this.maximalPointsWithoutPigs + (numOfBirds - 1) * 10000; }

	public void setInitialScene(Scene s) {
		this.initialScene = s;
		this.maximalPointsWithoutPigs = 0;
		if (s != null) {
			this.maximalPointsWithoutPigs = s.estimateMaximalPointsWithoutBirds();
			setSlingshotAndScaling(s.slingshot, s.scalingFactor);
		}
	}

	public void ditchInitialScene() {
		setInitialScene(null);
		initialTargets = null;
		numOfBirds = 0;
		numOfBirdsConfident = false;
		slingshot = null;
		scalingFactor = 1.005;
		// TODO: reset limited time update slingshot counter once implemented
		tree = new DecisionTree();
		executedShots = new ArrayList<>();
	}

	public void finishLevel(GameStateExtractor.GameState state, int score) {
		numberOfTimesPlayed++;
		setLevelState(state);

		// First put in the old best Score for the first round
		this.featureMap.put("max_score", this._bestScore);

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

		// set timestamp in level here so that deviation from actual remaining time is smaller
		this.setCurrentDeltaTimestamp(delta);

		// update costs of level
		this.setCosts(delta / 1000);

		CustomLogger.info("Level reward: " + this.getReward() + ", Costs: " +
			this.getCosts() + " seconds");

		

		if (PERFORMANCE_MEASUREMENT_ENABLED) {
			CustomLogger.info("Writing feature vector to csv file...");
			String featureVectorString = this.featureMap.entrySet()
				.stream().map(Map.Entry::toString).map(String::valueOf).collect(Collectors.joining(","));

			CustomLogger.info("featureVector = " + String.format("[%s]", featureVectorString));
			String strategyTagsString = this.strategyTags.stream()
				.collect(Collectors.joining(","));
			CustomLogger.info("strategyTags = " + String.format("[%s]", strategyTagsString));

			this.writeCsvFile("featureList.csv", state);
		}

		// New best score needs to be put in for the LevelSelection
		this.featureMap.put("max_score", this._bestScore);

		resetToUnloadedState();
	}

	public void setLevelState(GameStateExtractor.GameState state) {
		switch (state) {
		case WON:
		case LOST:
			// once won keep the level in that state no matter if we fail later
			if (_state != State.WON) {
				if (state == GameStateExtractor.GameState.WON)
					_state = State.WON;
				else
					_state = State.LOST;
      }
      break;
    default: 
      CustomLogger.info("Recieved unexpected LevelState");
		}
	}

	public final State getLevelState() {
		return _state;
	}

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

	/** Create a screenshot without UI and all pigs previous position masked */
	public BufferedImage screenshotWithoutMovingParts() {
		BufferedImage screenShot = ActionRobot.get().screenshotWithoutUI(slingshot.bounds.x + slingshot.bounds.width);
		Graphics2D g2d = screenShot.createGraphics();
		g2d.setColor(Color.darkGray);
		g2d.fillRect(800, 190, 40, 100); // right wiggling triangle
		try {
			List<ABObject> pigs = currentScene.getPigs();
			if (pigs != null) {
				for (ABObject pig : pigs) {
					Rectangle r = new Rectangle(pig.getCenter());
					// the eyes of grandpa and helmet are too far apart, thats why 0.9
					r.grow((int)(pig.width * 0.9), (int)(pig.height * 0.85));
					g2d.fillRect(r.x, r.y, r.width, r.height); // because pigs are blinking
				}
			}
		} catch (Exception e){
			CustomLogger.severe("[Level] error masking pigs, returning regular screenshot...");
		}
		return screenShot;
	}

	public void wasShotIneffective(){

	}

	public ShotPlanner getShotPlanner() {
		ABType birdType;
		try { birdType = currentScene.getBirds().get(0).getType(); }
		catch (Exception e) { birdType = ABType.RedBird; }
		return new ShotPlanner(this.slingshot, this.scalingFactor, birdType);
	}

	public void resetToUnloadedState() {
		this.currentScene = this.initialScene;
		this.executedShots = new ArrayList<>();
		this.tree.resetToRootNode();
	}

	public void calculateReachabilityForScene(Scene scene) {
		if (scene == null)
			scene = currentScene;
		if (scene != null && scene.getReachableTargets().isEmpty())
			scene.setReachabilityForAllBlocks(getShotPlanner());
	}

	public void addExecutedShot(Shot executedShot, Target target, int damagePoints) {
		executedShots.add(new Triplet<>(executedShot, target, damagePoints));
		CustomLogger.info("[Meta] Level " + levelId + ": shot " + executedShots.size()
				+ " added with " + damagePoints + " damage points");
	}

	public void writeCsvFile(String filename, GameStateExtractor.GameState gameState) {
		File f = null;

		try {
			f = new File(filename);
			if(!f.exists()){
				f.createNewFile();
			}
		} catch (NullPointerException e) {
			CustomLogger.severe("Could not open csv file because pathname is null!");
			return;
		} catch (IOException e) {
			CustomLogger.severe("Could not create new File \""+filename+"\" because "+e.getMessage());
			return;
		}

		StringBuilder builder = new StringBuilder();

		if(f.isFile() && f.length() == 0) {
			// write column list
			//String columnNameList = "num_pigs,num_birds,num_destroyable_objects,num_generated_shots,num_times_played,num_strategies,num_line_segments_hills,num_score,max_score,levelid,list_strategies,game_won";
			String columnNameList = String.join(",", this.featureMap.entrySet().stream()
			.map(Map.Entry::getKey).map(String::valueOf).collect(Collectors.toList())) +",list_strategies,game_won";
			builder.append(columnNameList + "\n");
		}

		// Now write content to csv file
		builder.append(
			String.join(",", this.featureMap.entrySet().stream()
				.map(Map.Entry::getValue).map(String::valueOf).collect(Collectors.toList()))
		);

		builder.append(
			"," + String.join(":", this.strategyTags)
		);

		builder.append("," + gameState + "\n");

		appendToFile(filename, builder.toString());
	}

	private void appendToFile(String filename, String string){
		try (FileWriter fw = new FileWriter(filename, true)) {
			fw.write(string);
			fw.close();
		} catch (NullPointerException e) {
			CustomLogger.severe("Could not open csv file because pathname is null!");
		} catch (IOException e) {
			CustomLogger.severe("Could not write to file " + filename + ": " + e.getMessage());
		}
	}

	public int calculateStrategiesWeightedSum() {
		HashMap<String, Integer> strategyOccurrence = new HashMap<String, Integer>(Prediction.STRATEGY_WEIGHTS.size());
        Set<String> strategySet;
        int strategiesWeightedSum = 0;

		// get the occurrence of each strategy in the strategy list
		for(String strategy : this.strategyTags) {
			if(Prediction.STRATEGY_WEIGHTS.get(strategy) != null && !strategyOccurrence.containsKey(strategy)) {
				int strategyCount = Collections.frequency(this.strategyTags, strategy);
				strategyOccurrence.put(strategy, strategyCount);
			}
		}

		// convert into set, so that we don't weight the same strategies multiply times later
		strategySet = new HashSet<String>(this.strategyTags);

		// calculate weighted sum of strategies
		for(String strategy : strategySet) {
			Integer weight = Prediction.STRATEGY_WEIGHTS.get(strategy);
			if(weight != null) {
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
		return "Level: " + levelId + " estimatedMaxPoints: " + getEstimatedMaximalPoints()
				+ " bestScore: " + _bestScore + " numberOfTimesPlayed: " + numberOfTimesPlayed;
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
    if(this.isDangerous && _state != State.WON && numberOfTimesPlayed > 1){
      _state = State.RESIGNED;
      return;
    }
    safetyMeasuresEnabled = true;
    this.isDangerous = isDangerous;
    this.ditchInitialScene();
  }
}
