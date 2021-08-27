package de.uniba.sme.bambirds.level_selection;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import de.uniba.sme.bambirds.common.objects.Level.State;
import de.uniba.sme.bambirds.common.utils.Settings;

public class LevelSelectionTest {

  private static Map<Integer, Map<String, Integer>> levelFeatures;
  private static Map<Integer, Integer> maxScores;
  private static Map<Integer, Long> costs;
  private static Map<Integer, State> levelStates;
  private static Prediction prediction;
  private static Decision decision;

  @BeforeAll
  public static void initialize() {
    Map<String, Integer> features1 = new HashMap<>();
    features1.put("num_pigs", 7);
    features1.put("num_birds", 3);
    features1.put("num_destroyable_objects", 45);
    features1.put("num_generated_shots", 178);
    features1.put("num_times_played", 1);
    features1.put("num_strategies", 6);
    features1.put("numerical_strategies", 26);
    features1.put("num_line_segments_hills", 0);
    features1.put("max_score", 49840);
    features1.put("levelid", 1);

    Map<String, Integer> features2 = new HashMap<>();
    features2.put("num_pigs", 5);
    features2.put("num_birds", 4);
    features2.put("num_destroyable_objects", 47);
    features2.put("num_generated_shots", 98);
    features2.put("num_times_played", 1);
    features2.put("num_strategies", 9);
    features2.put("numerical_strategies", 43);
    features2.put("num_line_segments_hills", 10);
    features2.put("max_score", 50850);
    features2.put("levelid", 2);

    Map<String, Integer> features3 = new HashMap<>();
    features3.put("num_pigs", 11);
    features3.put("num_birds", 5);
    features3.put("num_destroyable_objects", 47);
    features3.put("num_generated_shots", 250);
    features3.put("num_times_played", 1);
    features3.put("num_strategies", 53);
    features3.put("numerical_strategies", 40);
    features3.put("num_line_segments_hills", 5);
    features3.put("max_score", 0);
    features3.put("levelid", 3);

    Map<String, Integer> features4 = new HashMap<>();
    features4.put("num_pigs", 4);
    features4.put("num_birds", 4);
    features4.put("num_destroyable_objects", 27);
    features4.put("num_generated_shots", 90);
    features4.put("num_times_played", 1);
    features4.put("num_strategies", 8);
    features4.put("numerical_strategies", 52);
    features4.put("num_line_segments_hills", 23);
    features4.put("max_score", 0);
    features4.put("levelid", 4);

    Map<String, Integer> features5 = new HashMap<>();
    features5.put("num_pigs", 4);
    features5.put("num_birds", 4);
    features5.put("num_destroyable_objects", 69);
    features5.put("num_generated_shots", 554);
    features5.put("num_times_played", 1);
    features5.put("num_strategies", 36);
    features5.put("numerical_strategies", 0);
    features5.put("num_line_segments_hills", 5);
    features5.put("max_score", 70150);
    features5.put("levelid", 5);

    Map<String, Integer> features6 = new HashMap<>();
    features6.put("num_pigs", 4);
    features6.put("num_birds", 3);
    features6.put("num_destroyable_objects", 40);
    features6.put("num_generated_shots", 280);
    features6.put("num_times_played", 1);
    features6.put("num_strategies", 29);
    features6.put("numerical_strategies", 19);
    features6.put("num_line_segments_hills", 11);
    features6.put("max_score", 37020);
    features6.put("levelid", 6);

    Map<String, Integer> features7 = new HashMap<>();
    features7.put("num_pigs", 3);
    features7.put("num_birds", 4);
    features7.put("num_destroyable_objects", 31);
    features7.put("num_generated_shots", 142);
    features7.put("num_times_played", 1);
    features7.put("num_strategies", 3);
    features7.put("numerical_strategies", 13);
    features7.put("num_line_segments_hills", 12);
    features7.put("max_score", 33570);
    features7.put("levelid", 7);

    Map<String, Integer> features8 = new HashMap<>();
    features8.put("num_pigs", 5);
    features8.put("num_birds", 3);
    features8.put("num_destroyable_objects", 35);
    features8.put("num_generated_shots", 96);
    features8.put("num_times_played", 1);
    features8.put("num_strategies", 7);
    features8.put("numerical_strategies", 47);
    features8.put("num_line_segments_hills", 14);
    features8.put("max_score", 48010);
    features8.put("levelid", 8);

    levelFeatures = new HashMap<>();

    levelFeatures.put(1, features1);
    levelFeatures.put(2, features2);
    levelFeatures.put(3, features3);
    levelFeatures.put(4, features4);
    levelFeatures.put(5, features5);
    levelFeatures.put(6, features6);
    levelFeatures.put(7, features7);
    levelFeatures.put(8, features8);

    maxScores = new HashMap<>();
    maxScores.put(1, 49840);
    maxScores.put(2, 50850);
    maxScores.put(3, 0);
    maxScores.put(4, 0);
    maxScores.put(5, 70150);
    maxScores.put(6, 55800);
    maxScores.put(7, 33570);
    maxScores.put(8, 48010);

    levelStates = new HashMap<>();
    levelStates.put(1, State.WON);
    levelStates.put(2, State.WON);
    levelStates.put(3, State.LOST);
    levelStates.put(4, State.LOST);
    levelStates.put(5, State.WON);
    levelStates.put(6, State.WON);
    levelStates.put(7, State.WON);
    levelStates.put(8, State.WON);

    costs = new HashMap<>();
    costs.put(1, 125L);
    costs.put(2, 122L);
    costs.put(3, 65L);
    costs.put(4, 65L);
    costs.put(5, 67L);
    costs.put(6, 187L);
    costs.put(7, 148L);
    costs.put(8, 62L);

    decision = new Decision(true);
    Settings.START_LEVEL = 1;
    prediction = new Prediction(8,Prediction.ClassifierType.RANDOM_FOREST, Prediction.RegressorType.LINEAR_MODEL);
  }

  @Test
  public void predictionTest() {
    System.out.println("DecisionTreeManagerTest:");
    Map<Integer, PredictionTuple<Integer, Double>> predictions = prediction.predict(levelFeatures);
    System.out.println(predictions);
    assertEquals(8, predictions.size(), "Length of predictions should be 8");
  }

  @Test
  public void probabilityDistributionTest() {
    System.out.println("probabilityDistributionTest:");

    Map<Integer, PredictionTuple<Integer, Double>> predictions = new HashMap<>();
    predictions.put(1, new PredictionTuple<>(36303, 0.19402985074626866));
    predictions.put(2, new PredictionTuple<>(27478, 0.43902439024390244));
    predictions.put(3, new PredictionTuple<>(55121, 0.5));
    predictions.put(4, new PredictionTuple<>(36303, 0.7692307692307693));
    predictions.put(5, new PredictionTuple<>(27478, 1.0));
    predictions.put(6, new PredictionTuple<>(45800, 1.0));
    predictions.put(7, new PredictionTuple<>(27478, 1.0));
    predictions.put(8, new PredictionTuple<>(27478, 1.0));

    Map<Integer, Double> probabilities = decision.calculateProbabilityDistribution(maxScores, costs, predictions,
        levelStates, 300);
    System.out.println(probabilities);

    assertEquals(8, probabilities.values().size(), "Length of predictions should be 8");
  }

  @Test
  public void probabilityDistributionWithMuchTimeLeft() {
    System.out.println("probabilityDistributionWithMuchTimeLeft:");

    Map<Integer, PredictionTuple<Integer, Double>> predictions = new HashMap<>();
    predictions.put(1, new PredictionTuple<>(36303, 0.19402985074626866));
    predictions.put(2, new PredictionTuple<>(27478, 0.43902439024390244));
    predictions.put(3, new PredictionTuple<>(55121, 0.5));
    predictions.put(4, new PredictionTuple<>(36303, 0.7692307692307693));
    predictions.put(5, new PredictionTuple<>(27478, 1.0));
    predictions.put(6, new PredictionTuple<>(45800, 1.0));
    predictions.put(7, new PredictionTuple<>(27478, 1.0));
    predictions.put(8, new PredictionTuple<>(27478, 1.0));

    Map<Integer, Double> probabilities = decision.calculateProbabilityDistribution(maxScores, costs, predictions,
        levelStates, 1000);
    System.out.println(probabilities);

    assertEquals(8, probabilities.values().size(), "Length of predictions should be 8");
  }

  @Test
  public void probabilityDistributionWithLittleTimeLeft() {
    System.out.println("probabilityDistributionWithLittleTimeLeft:");

    Map<Integer, PredictionTuple<Integer, Double>> predictions = new HashMap<>();
    predictions.put(1, new PredictionTuple<>(36303, 0.19402985074626866));
    predictions.put(2, new PredictionTuple<>(27478, 0.43902439024390244));
    predictions.put(3, new PredictionTuple<>(55121, 0.5));
    predictions.put(4, new PredictionTuple<>(36303, 0.7692307692307693));
    predictions.put(5, new PredictionTuple<>(27478, 1.0));
    predictions.put(6, new PredictionTuple<>(45800, 1.0));
    predictions.put(7, new PredictionTuple<>(27478, 1.0));
    predictions.put(8, new PredictionTuple<>(27478, 1.0));

    Map<Integer, Double> probabilities = decision.calculateProbabilityDistribution(maxScores, costs, predictions,
        levelStates, 200);
    System.out.println(probabilities);

    assertEquals(8, probabilities.values().size(), "Length of predictions should be 8");
  }

  @Test
  public void probabilityDistributionWithResignedLevel() {
    System.out.println("probabilityDistributionWithResignedLevel:");

    Map<Integer, PredictionTuple<Integer, Double>> predictions = new HashMap<>();
    predictions.put(1, new PredictionTuple<>(36303, 0.19402985074626866));
    predictions.put(2, new PredictionTuple<>(27478, 0.43902439024390244));
    predictions.put(3, new PredictionTuple<>(55121, 0.5));
    predictions.put(4, new PredictionTuple<>(36303, 0.7692307692307693));
    predictions.put(5, new PredictionTuple<>(27478, 1.0));
    predictions.put(6, new PredictionTuple<>(45800, 1.0));
    predictions.put(7, new PredictionTuple<>(27478, 1.0));
    predictions.put(8, new PredictionTuple<>(27478, 1.0));

    Map<Integer, de.uniba.sme.bambirds.common.objects.Level.State> levelStates = new HashMap<>();
    levelStates.put(1, State.WON);
    levelStates.put(2, State.WON);
    levelStates.put(3, State.LOST);
    levelStates.put(4, State.RESIGNED);
    levelStates.put(5, State.WON);
    levelStates.put(6, State.WON);
    levelStates.put(7, State.WON);
    levelStates.put(8, State.WON);

    Map<Integer, Double> probabilities = decision.calculateProbabilityDistribution(maxScores, costs, predictions,
        levelStates, 500);
    System.out.println(probabilities);

    assertEquals(7, probabilities.values().size(), "Length of predictions should be 7");
  }

  @Test
  public void IntegrationTest() {
    System.out.println("IntegrationTest:");
    Map<Integer, PredictionTuple<Integer, Double>> predictions = prediction.predict(levelFeatures);
    System.out.println(predictions);
    Map<Integer, Double> probabilities = decision.calculateProbabilityDistribution(maxScores, costs, predictions,
        levelStates, 500);
    System.out.println(probabilities);
    assertEquals(8, probabilities.values().size(), "Length of predictions should be 8");
  }
  

  @Test
  public void levelSelectionCorrectTimeLimitInitialisation() {
	int initialTime = 30;
    LevelSelection ls = new LevelSelection(10, initialTime);
    assertEquals(initialTime * 60, ls.getRemainingTime(), "Time in LevelSelection should be in seconds");
    assertEquals(initialTime * 60, ls.getAction().getTimeLimit(), "Time in Action should be in seconds");
  }

}